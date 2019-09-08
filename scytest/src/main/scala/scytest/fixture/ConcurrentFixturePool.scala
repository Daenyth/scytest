package scytest
package fixture

import cats.data.{OptionT, StateT}
import cats.effect.concurrent.MVar
import cats.effect.{Bracket, Concurrent, Resource}
import cats.implicits._
import cats.{Applicative, ~>}
import fs2.Stream
import scytest.tracing.TraceT
import scytest.util.HGraph.Graph
import scytest.util.TagMap

// NB unsafe to cancel operations of this class, will likely leak resources. Probably OK since it's private
private[scytest] final class ConcurrentFixturePool[F[_]] private (
    knownFixtures: TagMap[Fixture[F, ?]],
    cache: MVar[F, TagMap[ConcurrentFixturePool.State[F, ?]]]
)(implicit F: Concurrent[F])
    extends FixturePool[TraceT[F, ?]] {
  private[this] val types = new ConcurrentFixturePool.Types[F]
  import types._
  private type TF[A] = TraceT[F, A]
  private type ST[A] = StateT[F, TagMap[State], A]
  private object ST {
    def get: ST[TagMap[State]] = StateT.get
    def inspect[B](f: TagMap[State] => B): ST[B] = StateT.inspect(f)
    def inspectF[B](f: TagMap[State] => F[B]): ST[B] = StateT.inspectF(f)
    def modify(f: TagMap[State] => TagMap[State]): ST[Unit] = StateT.modify(f)
    def liftF[A](fa: F[A]): ST[A] = StateT.liftF(fa)
    def whenA[A](cond: Boolean)(f: => ST[A]): ST[Unit] =
      Applicative[ST].whenA(cond)(f)
  }

  private val graph: Graph =
    Graph
      .build(knownFixtures.keys.map { t =>
        val fix = getFix(t)
        fix.tag.erase -> fix.dependencies
      })
      .right
      .get // TODO handle error

  private def getFix[T <: FixtureTag](tag: T): Fixture[F, tag.R] =
    knownFixtures.get[tag.R](tag)

  override def closeAll: TF[Unit] =
    closeLeak(LeakId.ProcessId)

  override def closeSuite(suiteId: Suite.Id): TF[Unit] =
    closeLeak(LeakId.SuiteId(suiteId))

  override def closeTest(testId: Test.Id): TF[Unit] =
    closeLeak(LeakId.TestId(testId))

  private def closeLeak(leakId: LeakId): TF[Unit] =
    withCache("close-leak", Map("leakId" -> leakId.toString)) {
      ST.get.flatMap { fxs =>
        val leaks: Map[FixtureTag, Leak[_]] =
          fxs.collectValues[Leak[_]](state => state.get(leakId)).toMap

        ST.whenA(leaks.nonEmpty) {
          graph.unfoldLeafs
            .flatMap(ns => Stream.emits(ns.toSeq))
            .collect { case n if leaks.contains(n.label) => n.label.erase }
            .evalMap(tag => close(tag, leakId, leaks(tag)))
            .compile
            .drain
        }
      }
    }

  private def withCache[A](
      operationName: String,
      tags: Map[String, String]
  )(f: ST[A]): TF[A] =
    TraceT.wrap("fixture-pool-mutex", tags + ("operation" -> operationName)) {
      for {
        fxs <- cache.take
        (updated, result) <- f.run(fxs)
        _ <- cache.put(updated)
      } yield result
    }

  private def close(tag: FixtureTag, id: LeakId, leak: Leak[_]): ST[Unit] =
    ST.liftF(leak.close) >>
      ST.modify(_.modify[tag.R](tag)(_ - id))

  override def get[R](
      suiteId: Suite.Id,
      testId: Test.Id,
      tag: FixtureTag.Aux[R]
  ): TF[R] =
    withCache(
      "get-resource",
      Map(
        "suiteId" -> suiteId.value.toString,
        "testId" -> testId.value.toString,
        "fixture" -> tag.name
      )
    ) {
      allocate(
        getFix(tag),
        suiteId,
        testId
      ).map(leak => leak.r)
    }

  private def findLeak[R](
      tag: FixtureTag.Aux[R],
      leakId: LeakId
  ): OptionT[ST, Leak[R]] =
    OptionT(ST.inspect(_.get(tag).get(leakId)))

  private def allocate[R](
      fix: Fixture[F, R],
      suiteId: Suite.Id,
      testId: Test.Id
  ): ST[Leak[R]] =
    findLeak(fix.tag, leakId(suiteId, testId, fix.tag.scope))
      .orElse(allocIfReady(fix, suiteId, testId))
      .getOrElseF {
        val node = graph
          .find(fix.tag)
          .getOrElse(
            sys.error("impossible: graph is missing known fixture")
          )

        val allocateDeps =
          graph
            .focusOnLeaf(node)
            .unfoldRoots
            .flatMap(ns => Stream.emits(ns.toSeq))
            .evalTap(node => allocate(getFix(node.label), suiteId, testId))
            .compile
            .drain
        allocateDeps >> allocate(fix, suiteId, testId)
      }

  /** Allocate `fix` if it isn't already, and all dependencies are ready */
  private def allocIfReady[R](
      fix: Fixture[F, R],
      suiteId: Suite.Id,
      testId: Test.Id
  ): OptionT[ST, Leak[R]] = {
    val tag = fix.tag
    val tagLeakId = leakId(suiteId, testId, tag.scope)

    def putLeak(leak: Leak[R]) =
      ST.modify(_.modify(tag)(_.updated(tagLeakId, leak)))

    OptionT {
      fix match {
        case rf: RootFixture[F, R] =>
          ST.liftF(Leak.of(rf.resource_))
            .flatMap(leak => putLeak(leak).as(leak.some))

        case _ =>
          val newLeak: ST[Option[Leak[R]]] =
            ST.inspectF { fxs =>
              collectDeps(fxs, fix, suiteId, testId)
                .traverse[F, Leak[R]] { d =>
                  Leak.of(fix.resource(d))
                }
            }

          newLeak.flatTap { maybeLeak =>
            maybeLeak.traverse_(leak => putLeak(leak))
          }
      }
    }
  }

  private def collectDeps[R](
      fxs: TagMap[State],
      fix: Fixture[F, R],
      suiteId: Suite.Id,
      testId: Test.Id
  ): Option[fix.dependencies.H] = {
    val collectLeaked: FixtureTag.Aux ~> Option =
      λ[FixtureTag.Aux ~> Option](
        tag => fxs.get(tag).get(leakId(suiteId, testId, tag.scope)).map(_.r)
      )
    fix.dependencies.extractRightM(collectLeaked)
  }

  private def leakId(
      suiteId: Suite.Id,
      testId: Test.Id,
      scope: FixtureScope
  ): LeakId = scope match {
    case FixtureScope.Process => LeakId.ProcessId
    case FixtureScope.Suite   => LeakId.SuiteId(suiteId)
    case FixtureScope.Test    => LeakId.TestId(testId)
  }
}

object ConcurrentFixturePool {

  def create[F[_]: Concurrent](
      fixtures: TagMap[Fixture[F, ?]]
  ): F[ConcurrentFixturePool[F]] =
    for {
      cache <- MVar[F].of(
        fixtures.mapEntries[State[F, ?]] { e =>
          e.map(_ => Map.empty: State[F, e.A])
        }
      )
    } yield new ConcurrentFixturePool[F](fixtures, cache)

  private[fixture] type State[F[_], R] = Map[LeakId, Leak[F, R]]

  /** An allocated resource that needs to be finalized at some point */
  private[fixture] case class Leak[F[_], R](r: R, close: F[Unit])

  object Leak {
    def of[F[_], R](
        resource: Resource[F, R]
    )(implicit F: Bracket[F, Throwable]): F[Leak[F, R]] =
      resource.allocated.map(t => Leak(t._1, t._2))
  }

  private class Types[F[_]: Applicative] {
    type State[R] = ConcurrentFixturePool.State[F, R]
    type Leak[R] = ConcurrentFixturePool.Leak[F, R]
    val Leak = ConcurrentFixturePool.Leak

  }
}

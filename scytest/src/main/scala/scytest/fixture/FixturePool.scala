package scytest
package fixture

import cats.data.{OptionT, StateT}
import cats.effect.concurrent.MVar
import cats.effect.{Bracket, Clock, Concurrent, Resource, Sync}
import cats.implicits._
import cats.{Applicative, ~>}
import scytest.util.{HGraph, TagMap}
import fs2.Stream

import scala.concurrent.duration.MILLISECONDS

private[scytest] trait FixturePool[F[_]] {

  /** Closes all fixtures */
  def closeAll: F[Unit]

  /** Closes all suite-scoped fixtures belonging to `suiteId` */
  def closeSuite(suiteId: Suite.Id): F[Unit]

  /** Closes all test-scoped fixtures belonging to `testId` */
  def closeTest(testId: Test.Id): F[Unit]

  /** Get a copy of the fixture, possibly reused.
    *
    * The fixture will be remain alive until a call to one of the `close*` methods closes it.
    */
  def get[R](suiteId: Suite.Id, testId: Test.Id, tag: FixtureTag.Aux[R]): F[R]
}

/** For debugging pool activity */
private[scytest] class LoggingPool[F[_]: Sync: Clock](
    base: FixturePool[F]
) extends FixturePool[F] {
  val closeAll: F[Unit] = log(s"closeAll") >> base.closeAll

  def closeSuite(suiteId: Suite.Id): F[Unit] =
    log(s"closing $suiteId") >> base.closeSuite(suiteId) >> log(
      s"closed $suiteId"
    )

  def closeTest(testId: Test.Id): F[Unit] =
    log(s"close $testId") >> base.closeTest(testId) >> log(s"closed $testId")

  override def get[R](
      suiteId: Suite.Id,
      testId: Test.Id,
      tag: FixtureTag.Aux[R]
  ): F[R] =
    log(s"get $suiteId / $testId / $tag") >>
      base.get(suiteId, testId, tag)

  private def log(s: String) =
    Clock[F]
      .realTime(MILLISECONDS)
      .flatMap(t => Sync[F].delay(println(s"$t - $s")))
}

// NB unsafe to cancel operations of this class, will likely leak resources. Probably OK since it's private
private[scytest] final class BasicPool[F[_]] private (
    knownFixtures: TagMap[Fixture[F, ?]],
    cache: MVar[F, TagMap[BasicPool.State[F, ?]]]
)(implicit F: Concurrent[F])
    extends FixturePool[F] {
  private[this] val types = new BasicPool.Types[F]
  import types._
  private type ST[A] = StateT[F, TagMap[State], A]
  private object ST {
    def get: ST[TagMap[State]] = StateT.get
    def inspect[B](f: TagMap[State] => B): ST[B] = StateT.inspect(f)
    def inspectF[B](f: TagMap[State] => F[B]): ST[B] = StateT.inspectF(f)
    def modify(f: TagMap[State] => TagMap[State]): ST[Unit] = StateT.modify(f)
    def liftF[A](fa: F[A]): ST[A] = StateT.liftF(fa)
  }

  private val graph: HGraph.Graph[FixtureTag.Aux] = {
    val b = HGraph.Graph.newBuilder(FTList)
    knownFixtures.keys.toList.foreach { t =>
      val fix = getFix(t)
      b.add(fix.tag, fix.dependencies)
    }
    b.build()
  }

  private def getFix[T <: FixtureTag](tag: T): Fixture[F, tag.R] =
    knownFixtures.get[tag.R](tag)

  def closeAll: F[Unit] =
    closeLeak(LeakId.ProcessId)

  def closeSuite(suiteId: Suite.Id): F[Unit] =
    closeLeak(LeakId.SuiteId(suiteId))

  def closeTest(testId: Test.Id): F[Unit] =
    closeLeak(LeakId.TestId(testId))

  private def closeLeak(leakId: LeakId): F[Unit] = withCache {
    ST.get.flatMap { fxs =>
      val leaks: Map[FixtureTag, Leak[_]] =
        fxs.collectValues[Leak[_]](state => state.get(leakId)).toMap

      graph.unfoldLeafs
        .flatMap(ns => Stream.emits(ns.toSeq))
        .collect { case n if leaks.contains(n.label) => n.label.erase }
        .evalMap(tag => close(tag, leakId, leaks(tag)))
        .compile
        .drain
    }
  }

  // TODO explore STM instead of MVar in order to lock less
  private def withCache[A](f: ST[A]): F[A] =
    for {
      fxs <- cache.take
      (updated, result) <- f.run(fxs)
      _ <- cache.put(updated)
    } yield result

  private def close(tag: FixtureTag, id: LeakId, leak: Leak[_]): ST[Unit] =
    ST.liftF(leak.close) >>
      ST.modify(_.modify[tag.R](tag)(_ - id))

  def get[R](
      suiteId: Suite.Id,
      testId: Test.Id,
      tag: FixtureTag.Aux[R]
  ): F[R] =
    withCache {
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
        val (roots, _) = graph.focusOnLeaf(node.id).extractRoots
        val allocateDeps =
          roots.toList.traverse_ { n =>
            allocate(getFix(n.label), suiteId, testId).void
          }
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
      ST.modify { fxs =>
        fxs.put(tag, fxs.get(tag).updated(tagLeakId, leak))
      }

    findLeak(tag, tagLeakId).orElseF {
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

object BasicPool {

  def create[F[_]: Concurrent](
      fixtures: TagMap[Fixture[F, ?]]
  ): F[BasicPool[F]] = {
    val t = new Types[F]
    import t._

    type E1[A] = TagMap.Entry.Aux[Fixture[F, ?], A]
    type E2[A] = TagMap.Entry.Aux[t.State, A]
    for {
      cache <- MVar[F].of(
        fixtures.mapE[t.State](
          new (E1 ~> E2) {
            def apply[R](fa: E1[R]): E2[R] = fa.map(_ => State.initial[R])
          }
        )
      )
    } yield new BasicPool[F](fixtures, cache)
  }

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
    type State[R] = BasicPool.State[F, R]
    type Leak[R] = BasicPool.Leak[F, R]
    val Leak = BasicPool.Leak

    object State {
      def initial[R]: State[R] = Map.empty
      val unit: State[Unit] = Map(
        LeakId.ProcessId -> Leak((), Applicative[F].unit)
      )
    }

  }
}

private[fixture] sealed abstract class LeakId(val scope: FixtureScope)
private[fixture] object LeakId {

  case class TestId(id: Test.Id) extends LeakId(FixtureScope.Test)
  case class SuiteId(id: Suite.Id) extends LeakId(FixtureScope.Suite)
  case object ProcessId extends LeakId(FixtureScope.Process)
}

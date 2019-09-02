package scytest
package fixture

import cats.data.{OptionT, StateT}
import cats.effect.concurrent.MVar
import cats.effect.{Bracket, Clock, Concurrent, Resource, Sync}
import cats.implicits._
import cats.{Applicative, ~>}
import scytest.util.HGraph

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
    log(s"close $suiteId") >> base.closeSuite(suiteId)

  def closeTest(testId: Test.Id): F[Unit] =
    log(s"close $testId") >> base.closeTest(testId)

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
    knownFixtures: TagMap[KnownFixture[F, ?]],
    cache: MVar[F, TagMap[BasicPool.State[F, ?]]]
)(implicit F: Concurrent[F])
    extends FixturePool[F] {
  private[this] val types = new BasicPool.Types[F]
  import types._
  private type ST[A] = StateT[F, TagMap[State], A]
  private object ST {
    def get: ST[TagMap[State]] = StateT.get
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
    knownFixtures.get[tag.R](tag).get

  def closeAll: F[Unit] =
    ???

  def closeSuite(suiteId: Suite.Id): F[Unit] =
    ???

  def closeTest(testId: Test.Id): F[Unit] =
    ???

  // TODO explore STM instead of MVar in order to lock less
  private def withCache[A](f: ST[A]): F[A] =
    for {
      fxs <- cache.take
      (updated, result) <- f.run(fxs)
      _ <- cache.put(updated)
    } yield result

  private def close[R](tag: FixtureTag.Aux[R], id: LeakId): ST[Unit] =
    findLeak[R](tag, id)
      .flatMap { leak =>
        val update = ST.liftF(leak.close) >> ST.modify(_.modify[R](tag)(_ - id))
        OptionT.liftF(update)
      }
      .value
      .void

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
    OptionT(ST.get.map(_.get(tag).get(leakId)))

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
  ): OptionT[ST, Leak[R]] = OptionT {
    val tagLeakId = leakId(suiteId, testId, fix.tag.scope)

    ST.get.flatMap { fxs =>
      val state = fxs.get(fix.tag)

      def putLeak(leak: Leak[R]) =
        ST.modify(_.put(fix.tag, state.updated(tagLeakId, leak)))

      OptionT.pure[ST](state.get(tagLeakId)).getOrElseF {
        fix match {
          case rf: RootFixture[F, R] =>
            ST.liftF(Leak.of(rf.resource_))
              .flatMap(leak => putLeak(leak).as(leak.some))
          case _ =>
            val newLeak: F[Option[Leak[R]]] =
              collectDeps(fxs, fix, suiteId, testId).traverse[F, Leak[R]] {
                d: fix.dependencies.H =>
                  Leak.of(fix.resource(d))
              }

            ST.liftF(newLeak)
              .flatTap { maybeLeak =>
                maybeLeak.traverse_(leak => putLeak(leak))
              }
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
      fixtures: TagMap[KnownFixture[F, ?]]
  ): F[BasicPool[F]] = {
    val t = new Types[F]
    import t._

    type E1[A] = TagMap.Entry.Aux[KnownFixture[F, ?], A]
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

sealed abstract class LeakId(val scope: FixtureScope)
object LeakId {

  case class TestId(id: Test.Id) extends LeakId(FixtureScope.Test)
  case class SuiteId(id: Suite.Id) extends LeakId(FixtureScope.Suite)
  case object ProcessId extends LeakId(FixtureScope.Process)
}

private[scytest] class TagMap[V[_]] private[TagMap] (
    private val map: Map[FixtureTag, TagMap.Entry[V]]
) {
  import TagMap.Entry

  def get[T](key: FixtureTag.Aux[T]): V[T] =
    map(key).value.asInstanceOf[V[T]]

  def getSome(key: FixtureTag): V[_] =
    map(key).value

  def put[T](key: FixtureTag.Aux[T], value: V[T]): TagMap[V] =
    new TagMap[V](map.updated[Entry[V]](key, Entry(key, value)))

  def modify[T](key: FixtureTag.Aux[T])(f: V[T] => V[T]): TagMap[V] =
    put(key, f(get(key)))

  def mapE[V2[_]](
      f: Entry.Aux[V, ?] ~> Entry.Aux[V2, ?]
  ): TagMap[V2] = {
    val newMap: Map[FixtureTag, Entry[V2]] =
      map.map {
        case (k, e) =>
          k -> f.apply[e.A](e).asInstanceOf[Entry[V2]]
      }
    new TagMap[V2](newMap)
  }

  def ++(other: TagMap[V]): TagMap[V] =
    new TagMap[V](map ++ other.map)

  def keys: Set[FixtureTag] = map.keySet
}

private[scytest] object TagMap {
  def empty[V[_]] = new TagMap[V](Map.empty[FixtureTag, Entry[V]])
  def of[V[_]](items: Entry[V]*): TagMap[V] =
    items.foldLeft(empty[V])((tm, e) => tm.put(e.key, e.value))

  trait Entry[V[_]] {
    type A
    def key: FixtureTag.Aux[A]
    def value: V[A]

    def map[V2[_]](f: V[A] => V2[A]): Entry.Aux[V2, A]
  }
  object Entry {
    def apply[V[_], A](key: FixtureTag.Aux[A], value: V[A]): Entry.Aux[V, A] =
      new Impl(key, value)

    implicit def fromTuple[V[_], A](kv: (FixtureTag.Aux[A], V[A])): Entry[V] =
      new Impl(kv._1, kv._2)

    type Aux[V[_], A0] = Entry[V] { type A = A0 }
    private class Impl[V[_], A0](
        val key: FixtureTag.Aux[A0],
        val value: V[A0]
    ) extends Entry[V] {
      type A = A0

      def map[V2[_]](f: V[A] => V2[A]): Aux[V2, A0] = Entry(key, f(value))
    }
  }
}

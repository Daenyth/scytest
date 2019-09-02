package scytest
package fixture

import cats.effect.concurrent.MVar
import cats.effect.{Bracket, Clock, Concurrent, Resource, Sync}
import cats.implicits._
import cats.{Applicative, ~>}
import scytest.fixture.FTList._
import scytest.fixture.FixtureTag.Aux

import scala.concurrent.duration.MILLISECONDS

private[scytest] trait FixturePool[F[_]] {

  def closeProcess(tag: FixtureTag): F[Unit]

  def closeSuite(
      tag: FixtureTag,
      closingScope: FixtureScope,
      suiteId: Suite.Id
  ): F[Unit]

  /** Closes any fixtures that were opened for `testId` in `suiteId` if those fixtures are eligible to be closed at `closingScope` */
  def closeTest(
      tags: FixtureTag,
      closingScope: FixtureScope,
      suiteId: Suite.Id,
      testId: Test.Id
  ): F[Unit]

  /** Get a copy of the fixture, possibly reused. The fixture will be alive until a call to `closeScope` happens with arguments eligible to close the resource. */
  def get[R](
      suiteId: Suite.Id,
      testId: Test.Id,
      tag: FixtureTag.Aux[R]
  ): F[R]
}

/** For debugging pool activity */
private[scytest] class LoggingPool[F[_]: Sync: Clock](
    base: FixturePool[F]
) extends FixturePool[F] {
  def closeProcess(tag: FixtureTag): F[Unit] =
    log(s"closeProcess $tag") >> base.closeProcess(tag)

  def closeSuite(
      tag: FixtureTag,
      closingScope: FixtureScope,
      suiteId: Suite.Id
  ): F[Unit] =
    log(s"close $tag in $closingScope for $suiteId") >>
      base.closeSuite(tag, closingScope, suiteId)

  override def closeTest(
      tag: FixtureTag,
      closingScope: FixtureScope,
      suiteId: Suite.Id,
      testId: Test.Id
  ): F[Unit] =
    log(s"close $tag in $closingScope for $suiteId / $testId") >>
      base.closeTest(tag, closingScope, suiteId, testId)

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

// NB unsafe to cancel operations of this class, will likely leak resources
private[scytest] class BasicPool[F[_]] private (
    knownFixtures: TagMap[KnownFixture[F, ?]],
    cache: MVar[F, TagMap[BasicPool.State[F, ?]]]
)(implicit F: Concurrent[F])
    extends FixturePool[F] {
  private[this] val types = new BasicPool.Types[F]
  import types._

  private def getFix[T <: FixtureTag](tag: T): Fixture[F, tag.R] =
    knownFixtures.get[tag.R](tag).get

  def closeProcess(tag: FixtureTag): F[Unit] = ???

  def closeSuite(
      tag: FixtureTag,
      closingScope: FixtureScope,
      suiteId: Suite.Id
  ): F[Unit] = ???

  def closeTest(
      tag: FixtureTag,
      closingScope: FixtureScope,
      suiteId: Suite.Id,
      testId: Test.Id
  ): F[Unit] = {
    val fix = getFix(tag)
    val deps = :::(fix.tag, fix.dependencies)
    val leakIds = deps
      .foldLeftVM(())(new LFold[F, Unit] {
        def apply[A](b: Unit, va: FixtureTag.Aux[A]): F[Unit] =
          if (va.scope != closingScope) F.unit
          else {

            val id = leakId(suiteId, testId, va.scope)

          }

      })
      .filter(_.scope == closingScope)
      .map(t => leakId(suiteId, testId, t.scope))
    // Need to close in the right order for test->test dependency management
    ???
  }

  private def close[R](
      fxs: TagMap[State],
      tag: FixtureTag.Aux[R],
      id: LeakId
  ): F[TagMap[State]] = {
    val state = fxs.get(tag)
    state.get(id) match {
      case Some(leak) => leak.close.as(fxs.put(tag, state - id))
      case None       => fxs.pure[F]
    }
  }

  def get[R](
      suiteId: Suite.Id,
      testId: Test.Id,
      tag: FixtureTag.Aux[R]
  ): F[R] =
    for {
      fxs <- cache.take
      (updated, leak) <- allocateAll(
        fxs,
        getFix(tag),
        suiteId,
        testId
      )
      _ <- cache.put(updated)
    } yield leak.r

  private def allocateAll[R](
      init: TagMap[State],
      initFix: Fixture[F, R],
      suiteId: Suite.Id,
      testId: Test.Id
  ): F[(TagMap[State], Leak[R])] = {
    val collectLeaked: FixtureTag.Aux ~> Option =
      λ[FixtureTag.Aux ~> Option](
        tag => init.get(tag).get(leakId(suiteId, testId, tag.scope)).map(_.r)
      )

    val allocator: RFold[F, TagMap[State]] = new RFold[F, TagMap[State]] {
      def apply[A](
          tag: FixtureTag.Aux[A],
          fxs: TagMap[State]
      ): F[TagMap[State]] = {
        val tagLeakId = leakId(suiteId, testId, tag.scope)
        val state = fxs.get(tag)
        val resolveDeps: F[TagMap[State]] = state.get(tagLeakId) match {
          case Some(_) =>
            fxs.pure[F]
          case None =>
            val alloc: F[TagMap[State]] = getFix(tag) match {
              case rf: RootFixture[F, A] =>
                Leak
                  .of(rf.resource_)
                  .map(leak => fxs.put(tag, state.updated(tagLeakId, leak)))

              case fix =>
                val fixDeps: Option[fix.dependencies.H] =
                  fix.dependencies.extractRightM(collectLeaked)
                val allocDeps: F[TagMap[State]] = fixDeps match {
                  case Some(liveDeps) =>
                    Leak
                      .of(fix.resource(liveDeps))
                      .map(leak => fxs.put(tag, state.updated(tagLeakId, leak)))
                  case None => ???
                }
                allocDeps
            }
            alloc
        }
        resolveDeps
      }
    }

    initFix.dependencies
      .foldRightVM(init)(allocator)
      .map { fxs =>
        val leak =
          fxs.get(initFix.tag)(leakId(suiteId, testId, initFix.tag.scope))
        (fxs, leak)
      }
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
        fixtures
          .mapE[t.State](
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

// TODO make something better combining chris D `Vault` + tpolecat skunk / `Pool`
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

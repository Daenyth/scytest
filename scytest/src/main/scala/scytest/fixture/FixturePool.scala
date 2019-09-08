package scytest
package fixture

import cats.effect.{Clock, Sync}
import cats.implicits._
import cats.tagless.FunctorK

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

private[scytest] object FixturePool {
  implicit val fixturePoolFunctorK: FunctorK[FixturePool] =
    cats.tagless.Derive.functorK
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

private[fixture] sealed abstract class LeakId(val scope: FixtureScope)
private[fixture] object LeakId {

  case class TestId(id: Test.Id) extends LeakId(FixtureScope.Test)
  case class SuiteId(id: Suite.Id) extends LeakId(FixtureScope.Suite)
  case object ProcessId extends LeakId(FixtureScope.Process)
}

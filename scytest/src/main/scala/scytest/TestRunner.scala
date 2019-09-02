package scytest

import cats.effect.implicits._
import cats.effect.{Concurrent, ContextShift, Timer}
import cats.implicits._
import cats.~>
import fs2.Stream
import scytest.fixture.{FixturePool, FixtureTag}

// TODO trace/span etc
class TestRunner[F[_]: Concurrent: ContextShift: Timer](
    pool: FixturePool[F]
) {

  def run(rootSuite: Suite[F]): Stream[F, TestResult] =
    Stream
      .chain(rootSuite.collected.suites)
      .map(runSuite)
      .parJoinUnbounded
      .onFinalize(pool.closeAll)

  // TODO suite-scope resources running in parallel will get shared and cross-terminated
  private def runSuite(suite: SingleSuite[F]): Stream[F, TestResult] =
    Stream
      .chain(suite.tests)
      .parEvalMapUnordered(Int.MaxValue) {
        case (testId, test) =>
          runTest(suite.id, testId, test)
      }
      .onFinalize(pool.closeSuite(suite.id))

  private def runTest(
      suiteId: Suite.Id,
      testId: Test.Id,
      test: Test[F]
  ): F[TestResult] = {
    val liveDeps: F[test.dependencies.H] =
      test.dependencies.extractRightM(
        λ[FixtureTag.Aux ~> F](tag => pool.get(suiteId, testId, tag))
      )
    liveDeps
      .flatMap(d => test.run(d))
      .guarantee(pool.closeTest(testId))
  }

}

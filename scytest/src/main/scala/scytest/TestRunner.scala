package scytest

import cats.data.{Chain, NonEmptyChain}
import cats.effect.{Concurrent, ContextShift, Timer}
import cats.implicits._
import cats.effect.implicits._
import cats.~>
import fs2.Stream

import scytest.fixture.{FixturePool, FixtureScope, FixtureTag}

class TestRunner[F[_]: Concurrent: ContextShift: Timer](
    pool: FixturePool[F]
) {

  def run(rootSuite: Suite[F]): Stream[F, TestResult] =
    Stream
      .chain(rootSuite.collected.suites)
      .map(runSuite)
      .parJoinUnbounded
      .onFinalize(
        fixtures(FixtureScope.Process, rootSuite.tests.map(_._2))
          .traverse_(tag => pool.closeProcess(tag))
      )

  // TODO suite-scope resources running in parallel will get shared and cross-terminated
  private def runSuite(suite: SingleSuite[F]): Stream[F, TestResult] =
    Stream
      .chain(suite.tests)
      .parEvalMapUnordered(Int.MaxValue) {
        case (testId, test) =>
          runTest(suite.id, testId, test)
      }
      .onFinalize(
        fixtures(FixtureScope.Suite, suite.tests.map(_._2))
          .traverse_(tag => pool.closeSuite(tag, FixtureScope.Suite, suite.id))
      )

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
      .guarantee(
        test.dependencies.existentially.traverse_(
          tag => pool.closeTest(tag, FixtureScope.Test, suiteId, testId)
        )
      )

  }

  private def fixtures(
      scope: FixtureScope,
      tests: NonEmptyChain[Test[F]]
  ): Chain[FixtureTag] =
    tests.toChain
      .flatMap(t => Chain.fromSeq(t.dependencies.existentially))
      .filter(_.scope == scope)

}

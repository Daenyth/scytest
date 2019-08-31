package scytest

import cats.data.{Chain, NonEmptyChain}
import cats.effect.{Concurrent, ContextShift, Timer}
import cats.implicits._
import cats.effect.implicits._
import fs2.Stream
import scytest.fixture.{FixtureScope, FixtureTag}

class TestRunner[F[_]: Concurrent: ContextShift: Timer](
    pool: FixturePool[F]
) {

  def run(rootSuite: Suite[F]): Stream[F, TestResult] =
    Stream
      .chain(rootSuite.collected.suites)
      .map(runSuite)
      .parJoinUnbounded
      .onFinalize(
        fixtures(FixtureScope.Process, rootSuite.tests)
          .traverse_(close(FixtureScope.Process))
      )

  private def runSuite(suite: SingleSuite[F]): Stream[F, TestResult] =
    Stream
      .chain(suite.tests)
      .parEvalMapUnordered(Int.MaxValue)(runTest)
      .onFinalize(
        fixtures(FixtureScope.Suite, suite.tests)
          .traverse_(close(FixtureScope.Suite))
      )

  private def runTest(test: Test[F]): F[TestResult] =
    pool.initialize(test.tag) >> pool
      .get(test.tag)
      .map(test.prepare)
      .flatMap(_.run)
      .guarantee(close(FixtureScope.Test)(test.tag).void)

  private def fixtures(
      scope: FixtureScope,
      tests: NonEmptyChain[Test[F]]
  ): Chain[FixtureTag] =
    tests.map(_.tag).filter(_.scope == scope)

  private def close(scope: FixtureScope)(tag: FixtureTag): F[Boolean] =
    pool.closeScope(tag, scope)
}

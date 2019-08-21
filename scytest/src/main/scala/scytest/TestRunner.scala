package scytest

import cats.data.{Chain, NonEmptyChain}
import cats.effect.concurrent.Semaphore
import cats.effect.{Concurrent, ContextShift, Timer}
import cats.implicits._
import fs2.Stream
import scytest.fixture.{FixtureScope, FixtureTag}

class TestRunner[F[_]: Concurrent: ContextShift: Timer](
    pool: FixturePool[F],
    gate: Semaphore[F]
) {

  def run(rootSuite: Suite[F]): Stream[F, TestResult] = {
    val processFixtures: Chain[FixtureTag] =
      fixtures(FixtureScope.Process, rootSuite.tests)

    initializeFixtures(processFixtures).flatMap { _ =>
      val singleSuites =
        Stream.fromIterator(rootSuite.collected.suites.iterator)
      singleSuites.map(runSuite).parJoinUnbounded
    }

  }

  private def runSuite(suite: SingleSuite[F]): Stream[F, TestResult] = {
    val suiteFixtures = fixtures(FixtureScope.Suite, suite.tests)
    initializeFixtures(suiteFixtures).flatMap { _: Unit =>
      Stream
        .fromIterator(suite.tests.iterator)
        .map { test =>
          Stream.eval(gate.withPermit(runTest(test)))
        }
        .parJoinUnbounded
    }
  }

  private def runTest(test: Test[F]): F[TestResult] =
    pool.bracket(test.tag).use { _ =>
      pool.get(test.tag).map(test.prepare).flatMap(_.run)
    }

  private def fixtures(
      scope: FixtureScope,
      tests: NonEmptyChain[Test[F]]
  ): Chain[FixtureTag] =
    tests.map(_.tag).filter(_.scope == scope)

  // TODO assert that this is size-1 stream
  private def initializeFixtures(
      processFixtures: Chain[FixtureTag]
  ): Stream[F, Unit] =
    if (processFixtures.isEmpty)
      Stream(())
    else
      Stream
        .fromIterator(processFixtures.iterator)
        .map(tag => Stream.resource(pool.bracket(tag)))
        .fold(Stream(()).covary[F]) { (s, s2) =>
          s.flatMap(_ => s2)
        }
        .flatten

}

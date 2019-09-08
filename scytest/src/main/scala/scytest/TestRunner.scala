package scytest

import cats.effect.implicits._
import cats.effect.{Concurrent, ContextShift, Timer}
import cats.implicits._
import cats.~>
import com.colisweb.tracing.{NoOpTracingContext, TracingContext}
import com.colisweb.tracing.implicits._
import fs2.Stream
import scytest.fixture.{FixturePool, FixtureTag}
import scytest.tracing._

class TestRunner[F[_]: Concurrent: ContextShift: Timer](
    pool: FixturePool[TraceT[F, ?]],
    tracer: TracingContext[F]
) {

  def run(
      rootSuite: Suite[F]
  ): Stream[F, (Suite.Id, Test.Id, TestResult)] =
    tracer.childStream("run-all") { tc =>
      Stream
        .chain(rootSuite.collected.suites)
        .map(runSuite(tc, _))
        .parJoinUnbounded
        .onFinalize(pool.closeAll.run(tc))
        .scope
    }

  private def runSuite(
      tc: TracingContext[F],
      suite: SingleSuite[F]
  ): Stream[F, (Suite.Id, Test.Id, TestResult)] =
    tc.childStream(
      "run-suite",
      Map("suiteId" -> suite.id.value.toString)
    ) { tc =>
      Stream
        .chain(suite.tests)
        .parEvalMapUnordered(Int.MaxValue) {
          case (testId, test) =>
            runTest(tc, suite.id, testId, test).map(
              result => (suite.id, testId, result)
            )
        }
        .onFinalize(pool.closeSuite(suite.id).run(tc))
        .scope
    }

  private def runTest(
      tc: TracingContext[F],
      suiteId: Suite.Id,
      testId: Test.Id,
      test: Test[F]
  ): F[TestResult] = {
    val liveDeps: F[test.dependencies.H] =
      test.dependencies.extractRightM(
        λ[FixtureTag.Aux ~> F](tag => pool.get(suiteId, testId, tag).run(tc))
      )

    val tags =
      Map(
        "suiteId" -> suiteId.value.toString,
        "testId" -> testId.value.toString
      )
    tc.childSpan("run-test", tags)
      .use { tc =>
        tc.childSpan("test-deps", tags)
          .wrap(liveDeps)
          .flatMap { d =>
            tc.childSpan("test-run")
              .wrap(test.run(d))
          }
          .guarantee(
            tc.childSpan("close-test-deps", tags)
              .wrap(pool.closeTest(testId).run(tc))
          )
      }
  }
}

object TestRunner {
  def apply[F[_]: Concurrent: ContextShift: Timer](
      pool: FixturePool[F]
  ): TestRunner[F] = {
    val tracer = NoOpTracingContext[F]
    new TestRunner(TraceT.liftAlgK(pool), tracer)
  }

  def apply[F[_]: Concurrent: ContextShift: Timer](
      pool: FixturePool[TraceT[F, ?]],
      tracer: TracingContext[F]
  ): TestRunner[F] = new TestRunner(pool, tracer)
}

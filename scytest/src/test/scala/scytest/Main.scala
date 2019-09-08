package scytest

import cats.data.{Chain, NonEmptyChain}
import cats.effect.{Clock, ContextShift, ExitCode, IO, IOApp, Resource, Timer}
import cats.implicits._
import com.colisweb.tracing.TracingContext
import scytest.fixture.{ConcurrentFixturePool, Fixture, LoggingPool}
import scytest.tracing.{BraveTracingContext, TraceT}
import scytest.util.TagMap

import scala.concurrent.duration.MILLISECONDS

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    val specs = Chain(
      new BasicSpec,
      new FixtureSpec
    )

    new Main(specs).run.as(ExitCode.Success)
  }
}

class Main(
    specs: Chain[Spec[IO]]
)(implicit cs: ContextShift[IO], timer: Timer[IO]) {
  private implicit val tracerClock: Clock[TraceT[IO, ?]] =
    Clock[IO].mapK(TraceT.liftK[IO])

  val suite: Suite[IO] =
    NonEmptyChain.fromChainUnsafe(specs).map[Suite[IO]](_.toSuite).reduce

  val fixtures: TagMap[Fixture[IO, ?]] =
    TagMap.fromChain(suite.fixtures.map(fix => fix.tag -> fix))

  private val tracer: Resource[IO, TracingContext[IO]] =
    BraveTracingContext.rootTracer[IO]().flatMap(_.newTrace("App"))

  val run: IO[Unit] =
    tracer.use { tc =>
      ConcurrentFixturePool
        .create[IO](fixtures)
        .flatMap { pool =>
          val runner = TestRunner[IO](new LoggingPool(pool), tc)

          runner
            .run(suite)
            .evalMap { tr =>
              log(tr.toString)
            }
            .compile
            .drain >> log("Finished")
        }
    }

  def log(s: String) =
    Clock[IO]
      .realTime(MILLISECONDS)
      .flatMap(t => IO(println(s"$t - $s")))
}

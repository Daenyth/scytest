package scytest

import cats.data.{Chain, NonEmptyChain}
import cats.effect.{Clock, ContextShift, ExitCode, IO, IOApp, Timer}
import cats.implicits._
import scytest.fixture.{BasicPool, Fixture, LoggingPool}
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

  val suite: Suite[IO] =
    NonEmptyChain.fromChainUnsafe(specs).map[Suite[IO]](_.toSuite).reduce

  val fixtures: TagMap[Fixture[IO, ?]] =
    TagMap.fromChain(suite.fixtures.map(fix => fix.tag -> fix))

  val run: IO[Unit] = {
    BasicPool.create[IO](fixtures).flatMap { pool =>
      val runner = new TestRunner[IO](new LoggingPool[IO](pool))

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

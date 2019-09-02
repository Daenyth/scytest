package scytest

import cats.data.{Chain, NonEmptyChain}
import cats.effect.{Clock, ContextShift, ExitCode, IO, IOApp, Timer}
import cats.implicits._
import scytest.fixture.{BasicPool, KnownFixture, LoggingPool}
import scytest.util.TagMap

import scala.concurrent.duration.MILLISECONDS

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    val specs = Chain(
      new BasicSpec
    )

    val kfs = new KnownFixtures[IO] {}

    new Main(specs, kfs.all).run.as(ExitCode.Success)
  }
}

class Main(
    specs: Chain[BasicSpec],
    fixtures: TagMap[KnownFixture[IO, ?]]
)(implicit cs: ContextShift[IO], timer: Timer[IO]) {

  val suite: Suite[IO] =
    NonEmptyChain.fromChainUnsafe(specs).map[Suite[IO]](_.toSuite).reduce

  val run: IO[Unit] = {
    BasicPool.create[IO](fixtures).flatMap { pool =>
      val runner = new TestRunner[IO](new LoggingPool[IO](pool))

      runner
        .run(suite)
        .evalMap { tr =>
          log(tr.toString)
        }
        .compile
        .drain
    }
  }

  def log(s: String) =
    Clock[IO]
      .realTime(MILLISECONDS)
      .flatMap(t => IO(println(s"$t - $s")))
}

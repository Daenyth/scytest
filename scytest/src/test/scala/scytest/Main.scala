package scytest

import cats.data.{Chain, NonEmptyChain}
import cats.effect.{ContextShift, ExitCode, IO, IOApp, Resource, Timer}
import scytest.fixture.{FixtureScope, FixtureTag}
import cats.implicits._

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    new Main().run.compile.drain.as(ExitCode.Success)
}

class Main(implicit cs: ContextShift[IO], timer: Timer[IO]) {
  val specs: Chain[BasicSpec] = Chain(
    new BasicSpec
  )

  val suite: Suite[IO] =
    NonEmptyChain.fromChainUnsafe(specs).map[Suite[IO]](_.toSuite).reduce

  val pool = new FixturePool[IO] {
    def initialize(tag: FixtureTag): IO[Unit] = log(s"init $tag")

    def shutdown(tag: FixtureTag): IO[Unit] = log(s"shutdown $tag")

    def closeScope(tag: FixtureTag, scope: FixtureScope): IO[Boolean] =
      log(s"close $scope / $tag").as(true)

    def bracket(tag: FixtureTag): Resource[IO, Unit] =
      Resource.make(initialize(tag))(_ => shutdown(tag))

    def get[R](tag: FixtureTag.Aux[R]): IO[R] =
      log(s"get $tag").as(null.asInstanceOf[R])
  }

  val runner = new TestRunner[IO](pool)

  val run = runner.run(suite)

  def log(s: String) = IO(println(s))
}

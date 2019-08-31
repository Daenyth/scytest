package scytest.fixture

import cats.effect.Sync
import cats.implicits._

class Debug[F[_]](implicit F: Sync[F]) extends Fixture[F, Debug[F]] {
  def get(tag: FixtureTag.Aux[Debug[F]]): F[Debug[F]] =
    log(s"get($tag)") >> F.unlessA(tag == this.tag)(
      F.raiseError(
        new IllegalArgumentException(s"bad tag $tag vs ${this.tag}")
      )
    ) >> F.pure(this)

  def initialize: F[Unit] = log("initialize")

  def shutdown: F[Unit] = log("shutdown")

  val tag: FixtureTag.Aux[Debug[F]] = Debug.tag

  private def log(s: String) = F.delay(println(s))
}

object Debug {

  def tag[F[_]]: FixtureTag.Aux[Debug[F]] =
    FixtureTag[Debug[F]](s"<Debug>", FixtureScope.Test)
}

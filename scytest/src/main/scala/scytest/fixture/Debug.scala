package scytest.fixture

import cats.effect.Sync
import cats.implicits._

trait Debug

object Debug {

  val tag: FixtureTag.Aux[Debug] =
    FixtureTag[Debug](s"<Debug>", FixtureScope.Test)
}

class DebugImpl[F[_]](implicit F: Sync[F])
    extends Fixture[F, Debug]
    with Debug {
  def get(tag: FixtureTag.Aux[Debug]): F[Debug] =
    log(s"get($tag)") >> F.unlessA(tag == this.tag)(
      F.raiseError(
        new IllegalArgumentException(s"bad tag $tag vs ${this.tag}")
      )
    ) >> F.pure(this)

  def initialize: F[Unit] = log("initialize")

  def shutdown: F[Unit] = log("shutdown")

  val tag: FixtureTag.Aux[Debug] = Debug.tag

  private def log(s: String) = F.delay(println(s))
}

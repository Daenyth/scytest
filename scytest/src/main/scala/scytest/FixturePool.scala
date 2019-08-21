package scytest

import cats.effect.Resource
import scytest.fixture.FixtureTag

trait FixturePool[F[_]] {
  def initialize(tag: FixtureTag): F[Unit]
  def shutdown(tag: FixtureTag): F[Unit]
  def bracket(tag: FixtureTag): Resource[F, Unit]

  def get[R](tag: FixtureTag.Aux[R]): F[R]
}

package scytest.fixture

trait Fixture[F[_], R] {
  def get(tag: FixtureTag.Aux[R]): F[R]
  def initialize: F[Unit]
  def shutdown: F[Unit]
}

trait KnownFixture[F[_], R] {
  def get: Fixture[F, R]
  def tag: FixtureTag.Aux[R]
}

trait LiveFixture[F[_]] {
  type R
  def get: F[R] // TODO can fail with "dead", should prune tests dependent on it
  def shutdown: F[Unit]
  def tag: FixtureTag.Aux[R]
}

object LiveFixture {
  type Aux[F[_], R0] = LiveFixture[F] { type R = R0 }
}

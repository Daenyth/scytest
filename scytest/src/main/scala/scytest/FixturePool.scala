package scytest

import cats.effect.Resource
import scytest.fixture.FixtureTag
import scytest.fixture.FixtureScope

trait FixturePool[F[_]] {
  def initialize(tag: FixtureTag): F[Unit]
  def shutdown(tag: FixtureTag): F[Unit]

  /** @return true if fixture was shut down */
  def closeScope(tag: FixtureTag, scope: FixtureScope): F[Boolean]
  def bracket(tag: FixtureTag): Resource[F, Unit]

  def get[R](tag: FixtureTag.Aux[R]): F[R]
}

package scytest.fixtures

import cats.{Applicative, Id}
import scytest.fixture.{Fixture, FixtureTag, KnownFixture}

object UnitFixture {
  val tag: FixtureTag.Aux[Unit] = fixture[Id].tag

  def fixture[F[_]: Applicative]: Fixture[F, Unit] =
    Fixture.pure[F, Unit](())("Unit")

  def known[F[_]: Applicative]: KnownFixture[F, Unit] =
    KnownFixture(UnitFixture.fixture[F])
}

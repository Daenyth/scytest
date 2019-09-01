package scytest

import cats.MonadError
import scytest.fixture._
import scytest.fixtures.UnitFixture

trait Test[F[_]] {
  type R
  def tag: FixtureTag.Aux[R]
  def name: String
  def run(resource: R): F[TestResult]
}

object Test {
  type Aux[F[_], R0] = Test[F] { type R = R0 }

  case class Id(value: String)

  def pass[F[_]](name: String)(implicit F: MonadError[F, Throwable]) =
    new FixturelessTest[F](name, F.pure(Verified))
}

final class FixtureTest[F[_], R0](
    val name: String,
    val tag: FixtureTag.Aux[R0],
    test: R0 => F[Assertion]
)(
    implicit F: MonadError[F, Throwable]
) extends Test[F] {
  type R = R0

  def run(resource: R): F[TestResult] =
    TestResult.from(name, test(resource))

}

final class FixturelessTest[F[_]](val name: String, test: F[Assertion])(
    implicit F: MonadError[F, Throwable]
) extends Test[F] {
  type R = Unit
  val tag: FixtureTag.Aux[Unit] = UnitFixture.tag

  def run(resource: Unit): F[TestResult] = run_

  private val run_ = TestResult.from(name, test)
}

case class RunnableTest[F[_]](run: F[TestResult])

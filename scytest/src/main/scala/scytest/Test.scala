package scytest

import cats.MonadError
import scytest.fixture._

trait Test[F[_]] {
  type R
  def prepare(resource: R): RunnableTest[F]
  def tag: FixtureTag.Aux[R]
  def name: String
}

final class FixtureTest[F[_], R0](
    val name: String,
    val tag: FixtureTag.Aux[R0],
    test: R0 => F[Assertion]
)(
    implicit F: MonadError[F, Throwable]
) extends Test[F] {
  type R = R0

  def prepare(resource: R): RunnableTest[F] =
    RunnableTest(TestResult.from(name, test(resource)))

}

final class FixturelessTest[F[_]](val name: String, test: F[Assertion])(
    implicit F: MonadError[F, Throwable]
) extends Test[F] {
  type R = Unit
  def tag: FixtureTag.Aux[Unit] = FixtureTag.unit

  def prepare(resource: Unit): RunnableTest[F] =
    RunnableTest(TestResult.from(name, test))
}

case class RunnableTest[F[_]](run: F[TestResult])

object Test {
  type Aux[F[_], R0] = Test[F] { type R = R0 }

  def pass[F[_]](name: String)(implicit F: MonadError[F, Throwable]) =
    new FixturelessTest[F](name, F.pure(Verified))
}

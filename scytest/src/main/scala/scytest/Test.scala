package scytest

import cats.MonadError
import scytest.fixture.FTList._

trait Test[F[_]] {
  type DTags <: TList
  val dependencies: DTags

  final type R = dependencies.H

  val name: String

  def run(resource: R): F[TestResult]
}

object Test {
  type Aux[F[_], R0] = Test[F] { type DTags = R0 }

  case class Id(value: String) {
    override def toString: String = s"Test.Id($value)"
  }

  def pass[F[_]](name: String)(implicit F: MonadError[F, Throwable]) =
    new FixturelessTest[F](name, F.pure(Verified))
}

final class FixtureTest[F[_], D <: TList, R0](
    val name: String,
    val dependencies: D
)(
    test: R0 => F[Assertion]
)(
    implicit F: MonadError[F, Throwable],
    ev: R0 =:= D#H
) extends Test[F] {
  val _ = ev
  type DTags = D
  def run(resource: R): F[TestResult] =
    TestResult.from(name, test(resource.asInstanceOf[R0]))

}

final class FixturelessTest[F[_]](val name: String, test: F[Assertion])(
    implicit F: MonadError[F, Throwable]
) extends Test[F] {

  type DTags = TNil
  val dependencies: DTags = TNil

  def run(resource: R): F[TestResult] = run_

  private val run_ = TestResult.from(name, test)
}

case class RunnableTest[F[_]](run: F[TestResult])

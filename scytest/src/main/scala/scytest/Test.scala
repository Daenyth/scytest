package scytest

import cats.MonadError
import cats.data.Chain
import scytest.fixture.FTList._
import scytest.fixture.Fixture

trait Test[F[_]] {
  type DTags <: TList
  val dependencies: DTags

  final type R = dependencies.H

  val name: String

  def run(resource: R): F[TestResult]

  // TODO can remove a lot of TList machinery if the test only takes one fixture instead of a list
  // Then `R` can be known as a single value and we can rely on the graph alone for producing transitive deps.
  // If `R` is a magnet-style HList of resource types we should be able to synthesize a ProductFixture
  def fixtures: Chain[Fixture[F, _]]
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
    val dependency: Fixture[F, R0] // TODO TList / magnet pattern for R0 detection
)(
    test: R0 => F[Assertion]
)(
    implicit F: MonadError[F, Throwable]
) extends Test[F] {

  type DTags = R0 ::: dependency.DTags
  val dependencies: DTags = :::(dependency.tag, dependency.dependencies)

  def run(resource: R): F[TestResult] =
    TestResult.from(name, test(resource.head))

  val fixtures: Chain[Fixture[F, _]] = Chain(dependency)
}

final class FixturelessTest[F[_]](val name: String, test: F[Assertion])(
    implicit F: MonadError[F, Throwable]
) extends Test[F] {

  type DTags = TNil
  val dependencies: DTags = TNil

  def run(resource: R): F[TestResult] = run_

  private val run_ = TestResult.from(name, test)

  val fixtures: Chain[Fixture[F, _]] = Chain.empty
}

case class RunnableTest[F[_]](run: F[TestResult])

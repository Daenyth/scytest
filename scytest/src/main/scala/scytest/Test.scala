package scytest

import cats.MonadError
import cats.data.Chain
import scytest.fixture.Fixture
import scytest.util.PolyUtil.{TagFn, toTag}
import scytest.util.TagList._
import shapeless.HList
import shapeless.ops.hlist

trait Test[F[_]] {
  val name: String

  /** The tags for the fixture dependencies used, with their type */
  type DTags
  val dependencies: DTags

  def fixtures: List[Fixture[F, _]]
  val tagFn: TagFn[DTags]

  def run(dependencies: tagFn.Objs): F[TestResult]

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
    override val name: String,
    val dependency: Fixture[F, R0] // TODO TList / magnet pattern for R0 detection
)(
    test: R0 => F[Assertion]
)(
    implicit F: MonadError[F, Throwable]
) extends Test[F] {

  type DTags = R0 ::: dependency.DTags
  override val dependencies: DTags =
    :::(dependency.tag, dependency.dependencies)

  def run(resource: R): F[TestResult] =
    TestResult.from(name, test(resource.head))

  override val fixtures: Chain[Fixture[F, _]] = Chain(dependency)
}

final class FixturelessTest[F[_]](
    override val name: String,
    test: => F[Assertion]
)(
    implicit F: MonadError[F, Throwable]
) extends Test[F] {

  type DTags = TNil
  override val dependencies: DTags = TNil

  def run(resource: R): F[TestResult] = run_

  private val run_ = TestResult.from(name, test)

  override val fixtures: Chain[Fixture[F, _]] = Chain.empty
}

case class RunnableTest[F[_]](run: F[TestResult])

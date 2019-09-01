package scytest.fixture

import cats.effect.Resource
import cats.implicits._
import cats.temp.par._
import cats.{Applicative, Functor, Monad}
import scytest.fixture.FTList._
import scytest.fixtures.UnitFixture

sealed trait Fixture[F[_], R] {

  def tag: FixtureTag.Aux[R]

  /** A precursor dependency needed to allocate this fixture's resource */
  type D
  val resource: D => Resource[F, R]

  /** The dependencies of this fixture along with their specific type. The specific type tags mentioned
    * may have types that differ from `D` when a `D` value is the result of a function from another dependency */
  type DTags <: NonEmptyTList

  def dependencies: DTags

}

/** A fixture that has no dependencies and can a root of the dependency DAG */
sealed trait RootFixture[F[_], R] extends Fixture[F, R] {
  final type D = Unit
  final type DTags = D :: TNil
  final val dependencies: DTags = UnitFixture.tag :: TNil

  protected val resource_ : Resource[F, R]
  final lazy val resource: D => Resource[F, R] =
    _ => resource_
}

object Fixture {

  /**
    * @tparam F Resource effect type
    * @tparam R The resource type returned by this fixture
    * @tparam D0 The runtime value of any precursor dependencies needed by this fixture
    * @tparam DT0 The tags which locate any precursor fixtures to produce `D0`
    */
  type Aux[F[_], R, D0, DT0] = Fixture[F, R] { type D = D0; type DTags = DT0 }

  def pure[F[_]: Applicative, R](value: R)(
      tagName: String = s"PureFixture($value)"
  ): Fixture[F, R] =
    new PureFixture[F, R](value, tagName)

  def from[F[_]: Monad, D, R](name: String, dependency: FixtureTag.Aux[D])(
      f: D => Resource[F, R]
  ): Fixture.Aux[F, R, D, D :: TNil] =
    new FlatMapFixture[F, D, R](name, dependency, f)

  def both[F[_], D1, D2, R](dep1: FixtureTag.Aux[D1], dep2: FixtureTag.Aux[D2])(
      f: (D1, D2) => Resource[F, R]
  ): Fixture.Aux[F, R, (D1, D2), D1 :: D2 :: TNil] =
    new MapNFixture[F, D1, D2, R]()

  def resource[F[_], R](
      resource: Resource[F, R],
      tag: FixtureTag.Aux[R]
  ): Fixture.Aux[F, R, Unit, Unit :: TNil] =
    new ResourceFixture[F, R](resource, tag)
}

case class KnownFixture[F[_], R](get: Fixture[F, R]) {
  val tag: FixtureTag.Aux[R] = get.tag
}

private[fixture] class ResourceFixture[F[_], R](
    protected val resource_ : Resource[F, R],
    val tag: FixtureTag.Aux[R]
) extends RootFixture[F, R]

private[fixture] class MapNFixture[F[_]: NonEmptyPar: Functor, D1, D2, R](
    fix1: FixtureTag.Aux[D1],
    fix2: FixtureTag.Aux[D2]
)(
    f: (D1, D2) => Resource[F, R]
) extends Fixture[F, R] {

  val resource = (dependency: ((D1, D2))) => f(dependency)

  type DTags = D1 :: D2 :: TNil

  val dependencies: DTags = fix1 :: fix2 :: TNil

  val tag: FixtureTag.Aux[R] =
    FixtureTag[R](
      s"ProductFixture(${fix1.name}, ${fix2.name})",
      fix1.scope min fix2.scope
    )

  type D = (D1, D2)

}

private[fixture] class FlatMapFixture[F[_]: Monad, D0, RGet](
    name: String,
    val dependency: FixtureTag.Aux[D0],
    f: D0 => Resource[F, RGet]
) extends Fixture[F, RGet] {

  type D = D0
  type DTags = D0 :: TNil

  def dependencies: DTags = dependency :: TNil

  val resource = (dependency: D) => f(dependency)

  def tag: FixtureTag.Aux[RGet] =
    FixtureTag(s"FlatMapFixture($dependency >>= $name)", dependency.scope)

}

private[fixture] class PureFixture[F[_], R](value: R, tagName: String)(
    implicit F: Applicative[F]
) extends RootFixture[F, R] {
  val tag: FixtureTag.Aux[R] = FixtureTag[R](tagName, FixtureScope.Process)
  protected val resource_ = Resource.pure[F, R](value)

}

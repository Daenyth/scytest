package scytest.fixture

import cats.effect.Resource
import cats.implicits._
import cats.{Applicative, Monad}
import scytest.util.TagList._
import shapeless.{::, HNil}

sealed abstract class Fixture[F[_], R] {

  val tag: FixtureTag.Aux[R]

  /** The dependencies of this fixture along with their specific type. */
  type DTags <: TList
  val dependencies: DTags

  final type D = dependencies.H

  /** Construct the resource from the initialized dependencies */
  def resource(dependency: D): Resource[F, R]

}

/** A fixture that has no dependencies and can a root of the dependency DAG */
sealed trait RootFixture[F[_], R] extends Fixture[F, R] {
  final type DTags = TNil
  final val dependencies: DTags = TNil

  val resource_ : Resource[F, R]
  final def resource(d: D): Resource[F, R] = resource_
}

object Fixture {

  /**
    * @tparam F Resource effect type
    * @tparam R The resource type returned by this fixture
    * @tparam D0 The tags which locate any precursor fixtures to produce this fixture's resource
    */
  type Aux[F[_], R, D0] = Fixture[F, R] { type DTags = D0 }

  def pure[F[_]: Applicative, R](value: R)(
      tagName: String = s"PureFixture($value)"
  ): Fixture[F, R] =
    new PureFixture[F, R](value, tagName)

  def from[F[_]: Monad, D, R](name: String, dependency: FixtureTag.Aux[D])(
      f: D => Resource[F, R]
  ): Fixture.Aux[F, R, D ::: TNil] =
    new FlatMapFixture[F, D, R](name, dependency, f)

  def both[F[_], D1, D2, R](
      dep1: FixtureTag.Aux[D1],
      dep2: FixtureTag.Aux[D2]
  )(
      f: (D1 :: D2 :: HNil) => Resource[F, R]
  ): Fixture.Aux[
    F,
    R,
    D1 ::: D2 ::: TNil
  ] =
    new MapNFixture[F, D1, D2, R](dep1, dep2)((d1, d2) => f(d1 :: d2 :: HNil))

  def resource[F[_], R](
      resource: Resource[F, R],
      tag: FixtureTag.Aux[R]
  ): Fixture.Aux[F, R, TNil] =
    new ResourceFixture[F, R](resource, tag)
}

private[fixture] class ResourceFixture[F[_], R](
    val resource_ : Resource[F, R],
    val tag: FixtureTag.Aux[R]
) extends RootFixture[F, R]

private[fixture] class MapNFixture[F[_], D1, D2, R](
    fix1: FixtureTag.Aux[D1],
    fix2: FixtureTag.Aux[D2]
)(
    f: (D1, D2) => Resource[F, R]
) extends Fixture[F, R] {

  type DTags = D1 ::: D2 ::: TNil

  val dependencies: DTags = fix1 ::: fix2 ::: TNil

  def resource(d: D): Resource[F, R] = f(d.head, d.tail.head)

  val tag: FixtureTag.Aux[R] =
    FixtureTag[R](
      s"ProductFixture(${fix1.name}, ${fix2.name})",
      fix1.scope min fix2.scope
    )

}

private[fixture] class FlatMapFixture[F[_]: Monad, D0, RGet](
    name: String,
    val dependency: FixtureTag.Aux[D0],
    f: D0 => Resource[F, RGet]
) extends Fixture[F, RGet] {

  type DTags = D0 ::: TNil

  val dependencies: DTags = dependency ::: TNil

  def resource(dependency: D) = f(dependency.head)

  val tag: FixtureTag.Aux[RGet] =
    FixtureTag(s"FlatMapFixture($dependency >>= $name)", dependency.scope)

}

private[fixture] class PureFixture[F[_], R](value: R, tagName: String)(
    implicit F: Applicative[F]
) extends RootFixture[F, R] {
  val tag: FixtureTag.Aux[R] = FixtureTag[R](tagName, FixtureScope.Process)
  val resource_ = Resource.pure[F, R](value)
}

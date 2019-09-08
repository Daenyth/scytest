package scytest.fixture

import cats.effect.{Bracket, ContextShift, Resource, Timer}
import cats.implicits._
import cats.{Applicative, ApplicativeError, Monad}
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

  final def withName(name: String): Fixture.Aux[F, R, DTags] =
    new RenamedFixture(this, name)

  def map[R2](f: R => R2)(
      implicit F: Applicative[F]
  ): Fixture.Aux[F, R2, DTags] =
    new MappedFixture(this, f)
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

  def pure[F[_]: Applicative, R](value: R): Fixture[F, R] =
    pure(value, s"PureFixture($value)")

  def pure[F[_]: Applicative, R](value: R, tagName: String): Fixture[F, R] =
    new PureFixture[F, R](value, tagName)

  def map[F[_]: Applicative, R, R2, DTags <: TList](
      fixture: Fixture.Aux[F, R, DTags]
  )(
      f: R => R2
  ): Fixture.Aux[F, R2, DTags] =
    new MappedFixture(fixture, f)

  def from[F[_]: Monad, D, R](name: String, dependency: FixtureTag.Aux[D])(
      f: D => Resource[F, R]
  ): Fixture.Aux[F, R, D ::: TNil] =
    new FlatMapFixture(name, dependency, f)

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
    new MapNFixture(dep1, dep2)((d1, d2) => f(d1 :: d2 :: HNil))

  def make[F[_], R](tagName: String, scope: FixtureScope)(acquire: F[R])(
      release: R => F[Unit]
  )(implicit F: Bracket[F, Throwable]): Fixture.Aux[F, R, TNil] =
    resource(FixtureTag[R](tagName, scope), Resource.make(acquire)(release))

  def resource[F[_], R](
      tag: FixtureTag.Aux[R],
      resource: Resource[F, R]
  ): Fixture.Aux[F, R, TNil] =
    new ResourceFixture[F, R](resource, tag)
}

private[fixture] class MappedFixture[F[_]: Applicative, R, R2, D0 <: TList](
    fixture: Fixture.Aux[F, R, D0],
    f: R => R2
) extends Fixture[F, R2] {

  val tag: FixtureTag.Aux[R2] =
    FixtureTag[R2](s"Mapped(${fixture.tag}, $f)", fixture.tag.scope)
  type DTags = D0
  val dependencies: DTags = fixture.dependencies

  def resource(dependency: D): Resource[F, R2] =
    fixture.resource(dependency.asInstanceOf[fixture.D]).map(f)
}

private[fixture] class RenamedFixture[F[_], R, D0 <: TList](
    base: Fixture.Aux[F, R, D0],
    name: String
) extends Fixture[F, R] {
  val tag: FixtureTag.Aux[R] = FixtureTag[R](name, base.tag.scope)
  type DTags = D0
  val dependencies: DTags = base.dependencies

  def resource(dependency: D): Resource[F, R] =
    base.resource(dependency.asInstanceOf[base.D])
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

// Note: This requires special handling from `FixturePool`
private[scytest] class ContextShiftFixture[F[_]](
    implicit F: ApplicativeError[F, Throwable]
) extends Fixture[F, ContextShift[F]] {
  val tag: FixtureTag.Aux[ContextShift[F]] = ContextShiftFixture.tag[F]

  type DTags = TNil
  val dependencies: DTags = TNil

  def resource(dependency: D): Resource[F, ContextShift[F]] =
    Resource.liftF(
      F.raiseError[ContextShift[F]](
        new Exception(
          "ContextShiftFixture requires special handling by FixturePool"
        )
      )
    )
}

object ContextShiftFixture {
  def tag[F[_]]: FixtureTag.Aux[ContextShift[F]] =
    FixtureTag[ContextShift[F]]("ContextShift", FixtureScope.Process)
}

// Note: This requires special handling from `FixturePool`
private[scytest] class TimerFixture[F[_]](
    implicit F: ApplicativeError[F, Throwable]
) extends Fixture[F, Timer[F]] {
  val tag: FixtureTag.Aux[Timer[F]] = TimerFixture.tag[F]

  type DTags = TNil
  val dependencies: DTags = TNil

  def resource(dependency: D): Resource[F, Timer[F]] =
    Resource.liftF(
      F.raiseError[Timer[F]](
        new Exception("TimerFixture requires special handling by FixturePool")
      )
    )
}

object TimerFixture {
  def tag[F[_]]: FixtureTag.Aux[Timer[F]] =
    FixtureTag[Timer[F]]("Timer", FixtureScope.Process)
}

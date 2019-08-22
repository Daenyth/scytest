package scytest.fixture

import cats.effect.Resource
import cats.{Applicative, Functor, Monad}
import cats.implicits._
import cats.temp.par._
import scytest.fixture.FixtureTag.Aux

trait Fixture[F[_], R] {
  def get(tag: FixtureTag.Aux[R]): F[R]
  def initialize: F[Unit]
  def shutdown: F[Unit]
  def tag: FixtureTag.Aux[R]
}

object Fixture {
  def pure[F[_]: Applicative, R](value: R): Fixture[F, R] =
    new PureFixture[F, R](value)

  def from[F[_]: Monad, R1, R2](dependency: Fixture[F, R1])(
      f: R1 => Fixture[F, R2]
  ): Fixture[F, R2] =
    new FlatMapFixture[F, R1, R2](dependency, f)
}

trait KnownFixture[F[_], R] {
  def get: Fixture[F, R]
}

object KnownFixture {
  implicit def knownProduct[F[_]: NonEmptyPar: Functor, R1, R2](
      implicit k1: KnownFixture[F, R1],
      k2: KnownFixture[F, R2]
  ): KnownFixture[F, (R1, R2)] =
    new KnownFixture[F, (R1, R2)] {
      def get: Fixture[F, (R1, R2)] = new ProductFixture(k1.get, k2.get)
    }
}

private[fixture] class ResourceFixture[F[_], R](
    resource: Resource[F, R],
    val tag: FixtureTag.Aux[R]
) extends Fixture[F, R] {
  val _ = resource
  def get(tag: Aux[R]): F[R] = ???

  def initialize: F[Unit] = ???

  def shutdown: F[Unit] = ???

}

private[fixture] class ProductFixture[F[_]: NonEmptyPar: Functor, R1, R2](
    fix1: Fixture[F, R1],
    fix2: Fixture[F, R2]
) extends Fixture[F, (R1, R2)] {

  def get(tag: FixtureTag.Aux[(R1, R2)]): F[(R1, R2)] =
    (fix1.get(fix1.tag), fix2.get(fix2.tag)).parTupled

  val shutdown: F[Unit] =
    (fix1.shutdown, fix2.shutdown).parTupled.void

  val tag: FixtureTag.Aux[(R1, R2)] =
    FixtureTag[(R1, R2)](
      s"(${fix1.tag}, ${fix2.tag})",
      fix1.tag.scope min fix2.tag.scope
    )

  val initialize: F[Unit] =
    (fix1.initialize, fix2.initialize).parTupled.void
}

private[fixture] class FlatMapFixture[F[_]: Monad, RDep, RGet](
    dependency: Fixture[F, RDep],
    f: RDep => Fixture[F, RGet]
) extends Fixture[F, RGet] {
  def get(tag: FixtureTag.Aux[RGet]): F[RGet] = ???

  def initialize: F[Unit] =
    // TODO need to cache `rd` and `f(rd)` in a ref
    dependency.initialize >>
      dependency.get(dependency.tag).flatMap(rd => f(rd).initialize)

  def shutdown: F[Unit] = ???

  def tag: FixtureTag.Aux[RGet] = ???
}

private[fixture] class PureFixture[F[_], R](value: R)(
    implicit F: Applicative[F]
) extends Fixture[F, R] {

  def get(tag: Aux[R]): F[R] = valueF

  private[this] val valueF = F.pure(value)

  val initialize: F[Unit] = F.unit

  val shutdown: F[Unit] = F.unit

  def tag: FixtureTag.Aux[R] =
    FixtureTag[R](s"PureFixture($value)", FixtureScope.Process)
}

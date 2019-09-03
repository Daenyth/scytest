package scytest

import cats.data.{Chain, NonEmptyChain}
import cats.implicits._
import cats.kernel.CommutativeSemigroup
import scytest.fixture.Fixture

sealed trait Suite[F[_]] {

  def tests: NonEmptyChain[(Test.Id, Test[F])]

  def combine(other: Suite[F]): Suite[F]

  def collected: Collected[F]

  lazy val fixtures: Chain[Fixture[F, _]] = tests.toChain.flatMap(_._2.fixtures)
}

object Suite {
  case class Id(value: String) {
    override def toString: String = s"Suite.Id($value)"
  }
  implicit def suiteSemigroup[F[_]]: CommutativeSemigroup[Suite[F]] =
    _.combine(_)
}

case class SingleSuite[F[_]](
    id: Suite.Id,
    tests: NonEmptyChain[(Test.Id, Test[F])]
) extends Suite[F] {

  def combine(other: Suite[F]): Suite[F] = other match {
    case s: SingleSuite[F] => Collected(NonEmptyChain(this, s))
    case Collected(suites) => Collected(this +: suites)
  }

  def collected: Collected[F] = Collected(NonEmptyChain(this))
}

case class Collected[F[_]](suites: NonEmptyChain[SingleSuite[F]])
    extends Suite[F] {
  override def tests: NonEmptyChain[(Test.Id, Test[F])] =
    suites.reduceMapK(_.tests)

  def combine(other: Suite[F]): Suite[F] = other match {
    case s: SingleSuite[F] => Collected(suites :+ s)
    case Collected(suites) => Collected(this.suites ++ suites)
  }

  def collected: Collected[F] = this
}

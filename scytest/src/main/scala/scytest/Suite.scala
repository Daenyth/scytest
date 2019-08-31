package scytest

import cats.data.NonEmptyChain
import cats.implicits._
import cats.kernel.CommutativeSemigroup

sealed trait Suite[F[_]] {

  def tests: NonEmptyChain[Test[F]]

  def combine(other: Suite[F]): Suite[F]

  def collected: Collected[F]
}

object Suite {
  implicit def suiteSemigroup[F[_]]: CommutativeSemigroup[Suite[F]] =
    _.combine(_)
}

case class SingleSuite[F[_]](name: String, tests: NonEmptyChain[Test[F]])
    extends Suite[F] {

  def combine(other: Suite[F]): Suite[F] = other match {
    case s: SingleSuite[F] => Collected(NonEmptyChain(this, s))
    case Collected(suites) => Collected(this +: suites)
  }

  def collected: Collected[F] = Collected(NonEmptyChain(this))
}

case class Collected[F[_]](suites: NonEmptyChain[SingleSuite[F]])
    extends Suite[F] {
  override def tests: NonEmptyChain[Test[F]] = suites.reduceMapK(_.tests)

  def combine(other: Suite[F]): Suite[F] = other match {
    case s: SingleSuite[F] => Collected(suites :+ s)
    case Collected(suites) => Collected(this.suites ++ suites)
  }

  def collected: Collected[F] = this
}

package scytest

import scytest.fixture._

trait Test[F[_]] {
  type R
  def prepare(resource: R): RunnableTest[F]
  def tag: FixtureTag.Aux[R]
}

final class FixturelessTest[F[_]](test: RunnableTest[F]) extends Test[F] {
  type R = Unit
  def tag: FixtureTag.Aux[Unit] = FixtureTag.unit
  def prepare(resource: Unit): RunnableTest[F] = test
}

trait RunnableTest[F[_]] {
  def run: F[TestResult]
}

object Test {
  type Aux[F[_], R0] = Test[F] { type R = R0 }
}

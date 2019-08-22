package scytest

import cats.implicits._
import cats.{Applicative, Monad}
import scytest.fixture.KnownFixture

abstract class Spec[F[_]: Monad] {
  def test(name: String)(body: F[Assertion]): FixturelessTest[F] = {
    val result = body.map(assertion => TestResult)
    new FixturelessTest[F](RunnableTest(body))
  }

  def testWith[R](name: String)(body: F[Assertion])(
      implicit ev: KnownFixture[F, R]
  ): Test.Aux[F, R]

  protected implicit def pureAssertion(assertion: Assertion)(
      F: Applicative[F]
  ): F[Assertion] = F.pure(assertion)
}

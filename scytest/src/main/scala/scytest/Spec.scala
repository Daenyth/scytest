package scytest

import scytest.fixture.KnownFixture

trait Spec[F[_]] {
  def test(name: String)(body: F[Assertion]): FixturelessTest[F]

  def testWith[R](name: String)(body: F[Assertion])(
      implicit ev: KnownFixture[F, R]
  ): Test.Aux[F, R]
}

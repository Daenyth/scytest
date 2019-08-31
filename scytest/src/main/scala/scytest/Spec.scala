package scytest

import cats.MonadError
import scytest.fixture.KnownFixture

abstract class Spec[F[_]](val name: String = getClass.getSimpleName)(
    implicit F: MonadError[F, Throwable]
) {
  def suite: List[Test[F]]

  protected def test(name: String)(body: F[Assertion]): Test[F] =
    new FixturelessTest[F](name, body)

  protected def testWith[R](name: String)(body: R => F[Assertion])(
      implicit ev: KnownFixture[F, R]
  ): Test[F] = new FixtureTest[F, R](name, ev.tag, body)

  protected final implicit def pureAssertion(
      assertion: Assertion
  ): F[Assertion] =
    F.pure(assertion)

}

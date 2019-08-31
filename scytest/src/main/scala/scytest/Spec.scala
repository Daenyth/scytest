package scytest

import cats.MonadError
import scytest.fixture.KnownFixture
import cats.data.NonEmptyChain

abstract class Spec[F[_]](val name: String = getClass.getSimpleName)(
    implicit F: MonadError[F, Throwable]
) extends SpecMethods[F] {
  def tests: List[Test[F]]

  def toSuite =
    new SingleSuite[F](
      name,
      NonEmptyChain
        .fromSeq(tests)
        .getOrElse(NonEmptyChain.one(Test.pass[F](s"Empty suite $name")))
    )

  protected final def test(name: String)(body: F[Assertion]): Test[F] =
    new FixturelessTest[F](name, body)

  protected final def testWith[R](name: String)(body: R => F[Assertion])(
      implicit ev: KnownFixture[F, R]
  ): Test[F] = new FixtureTest[F, R](name, ev.tag, body)

  protected final implicit def pureAssertion(
      assertion: Assertion
  ): F[Assertion] =
    F.pure(assertion)

}

trait SpecMethods[F[_]] {
  protected def all(tests: Test[F]*): List[Test[F]] = tests.toList
}

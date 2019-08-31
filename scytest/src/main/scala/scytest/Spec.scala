package scytest

import cats.implicits._
import scytest.fixture.KnownFixture
import cats.data.NonEmptyVector
import cats.MonadError
import scala.util.control.NonFatal

abstract class Spec[F[_]](implicit F: MonadError[F, Throwable]) {

  protected def test(name: String)(body: F[Assertion]): FixturelessTest[F] =
    new FixturelessTest[F](RunnableTest(runBody(name, body)))

  protected def testWith[R](name: String)(body: F[Assertion])(
      implicit ev: KnownFixture[F, R]
  ): Test.Aux[F, R]

  protected implicit def pureAssertion(assertion: Assertion): F[Assertion] =
    F.pure(assertion)

  private[this] def runBody(name: String, body: F[Assertion]): F[TestResult] =
    body
      .map {
        case Verified => TestResult.Success(name)
        case f: FailedAssertion =>
          TestResult.Failed(name, NonEmptyVector.one(f))
      }
      .recover {
        case NonFatal(ex) =>
          TestResult.Error(name, NonEmptyVector.one(ex))
      }
}

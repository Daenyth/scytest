package scytest

import cats.effect.IO
import scytest.Test.Aux
import scytest.fixture.KnownFixture

class BasicSpec extends Spec[IO] {
  def suite: List[Test[IO]] = ???

  protected def testWith[R](name: String)(body: IO[Assertion])(
      implicit ev: KnownFixture[IO, R]
  ): Aux[IO, R] = ???
}

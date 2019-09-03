package scytest

import cats.effect.IO
import scytest.fixture.Fixture
import shapeless.tag.@@

class FixtureSpec extends Spec[IO] {
  def tests: List[Test[IO]] = all(
    testWith[Int @@ Pure1]("pure value", pure1Fix) { int =>
      assert(int == 42)
    }
  )

  private val pure1Fix: Fixture[IO, Int @@ Pure1] =
    Fixture.pure[IO, Int @@ Pure1](shapeless.tag[Pure1](42))()

}

trait Pure1

package scytest

import cats.effect.IO
import scytest.fixture.Fixture
import scala.concurrent.duration._

class FixtureSpec extends Spec[IO] {
  def tests: List[Test[IO]] = all(
    testWith("pure value", pure1Fix) { int =>
      assert(int == 42)
    },
    testWith("timer sleep", fixtures.timer[IO]) { timer =>
      timer.sleep(10.millis)
    },
    testWith("cs shift", fixtures.contextShift[IO]) { cs =>
      cs.shift
    },
    testWith("blocker", fixtures.blocker, fixtures.contextShift[IO]) {
      (blocker, cs) =>
        blocker.blockOn(IO(Thread.sleep(75)))(cs)
    }
  )

  private val pure1Fix: Fixture[IO, Int] =
    Fixture.pure(42)

}

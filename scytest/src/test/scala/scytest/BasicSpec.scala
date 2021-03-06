package scytest

import cats.effect.IO

class BasicSpec extends Spec[IO] {
  def tests = all(
    Test.pass[IO]("ok"),
    test("simple assert") { assert(true) },
    test("fail")(assert(false)),
    test("throwing")(throw new Exception("throwing test"))
  )

}

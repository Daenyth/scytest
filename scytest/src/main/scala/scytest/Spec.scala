package scytest

import java.util.UUID

import cats.MonadError
import cats.data.NonEmptyChain
import cats.implicits._
import scytest.fixture.Fixture
import scytest.util.PolyUtil.{TagFn, fixtureToTag, onlyFixtures, toTag}
import shapeless.HList
import shapeless.ops.hlist.Mapper
import shapeless.ops.tuple

abstract class Spec[F[_]](
    implicit F: MonadError[F, Throwable]
) extends SpecMethods[F]
    with AssertionMethods {
  def name: String = this.getClass.getSimpleName
  protected def tests: List[Test[F]]

  lazy val toSuite =
    new SingleSuite[F](
      Suite.Id(s"$name - ${newId()}"),
      NonEmptyChain
        .fromSeq(tests.map(t => Test.Id(s"${t.name} - ${newId()}") -> t))
        .getOrElse(
          NonEmptyChain
            .one(Test.Id(newId()) -> Test.pass[F](s"Empty suite $name"))
        )
    )

  protected final def test(name: String)(body: => F[Assertion]): Test[F] =
    new FixturelessTest[F](name, body)

  protected final def testWith[R](
      name: String,
      fix: Fixture[F, R]
  )(
      body: R => F[Assertion]
  ): Test[F] =
    new FixtureTest(name, fix)(body)

  protected final def testWith[R1, R2](
      name: String,
      fix1: Fixture[F, R1],
      fix2: Fixture[F, R2]
  )(
      body: (R1, R2) => F[Assertion]
  ): Test[F] =
    new FixtureTest(name, Fixture.tupled[F, R1, R2](fix1, fix2))(
      body.tupled
    )

  protected final def testWith[P <: Product](
      name: String,
      fixtures: P
  )(
      implicit productIsFixturesEvidence: tuple.Mapper[P, onlyFixtures.type],
      tagFn: TagFn[P]
  ): (tagFn.Objs => F[Assertion]) => Test[F] = { body =>
    new FixtureTest(name, Fixture.tupled[F, R1, R2](fix1, fix2))(
      body.tupled
    )
  }

  protected final implicit def pureAssertion(
      assertion: Assertion
  ): F[Assertion] =
    F.pure(assertion)

  protected final implicit def unitAssertion(unit: Unit): F[Assertion] = {
    val _ = unit
    F.pure(success)
  }

  protected final implicit def unitFAssertion(unit: F[Unit]): F[Assertion] =
    unit.as(success)

  private def newId() = UUID.randomUUID().toString
}

trait SpecMethods[F[_]] {
  protected def all(tests: Test[F]*): List[Test[F]] = tests.toList
}

trait AssertionMethods {
  // TODO source code position macro, replace with expecty, etc
  def assert(cond: Boolean): Assertion =
    if (cond) success else FailedAssertion(new AssertionError())

  def fail(msg: String): Assertion = FailedAssertion(new AssertionError(msg))

  val success: Assertion = Verified
}

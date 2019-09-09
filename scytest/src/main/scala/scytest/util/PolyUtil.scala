package scytest.util

import scytest.fixture.{Fixture, FixtureTag}
import shapeless._

object PolyUtil {

  object onlyFixtures extends Poly1 {
    implicit def isFixture[F[_], R] = at[Fixture[F, R]](identity)
  }

  /** Extract FixtureTags from poly-mappable-things */
  trait fixtureToTag extends Poly1 {
    implicit final def fromFixture[F[_], R] = at[Fixture[F, R]](_.tag)
  }
  object fixtureToTag extends fixtureToTag
  object toTag extends fixtureToTag {
    implicit def tagIdentity[R] = at[FixtureTag.Aux[R]](identity)
  }

  /** Given a product of FixtureTags, find the type of those resolved fixtures */
  sealed abstract class TagFn[Tags] { type Objs }

  object TagFn {
    implicit val fn0: TagFn[Unit] = new TagFn[Unit] {
      type Objs = Unit
    }
    implicit val fnHNil: TagFn[HNil] = new TagFn[HNil] {
      type Objs = HNil
    }
    implicit def fn1[A]: TagFn[FixtureTag.Aux[A]] =
      new TagFn[FixtureTag.Aux[A]] {
        type Objs = A
      }
    implicit def fn2[A, B]: TagFn[(FixtureTag.Aux[A], FixtureTag.Aux[B])] =
      new TagFn[(FixtureTag.Aux[A], FixtureTag.Aux[B])] {
        type Objs = (A, B)
      }
    implicit def fn2[A, B, C]
        : TagFn[(FixtureTag.Aux[A], FixtureTag.Aux[B], FixtureTag.Aux[C])] =
      new TagFn[(FixtureTag.Aux[A], FixtureTag.Aux[B], FixtureTag.Aux[C])] {
        type Objs = (A, B, C)
      }
  }

}

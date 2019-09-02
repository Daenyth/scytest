package scytest

import cats.Applicative
import scytest.fixture.{KnownFixture, TagMap}

abstract class KnownFixtures[F[_]: Applicative] {
  // Subclass with `super.all ++ TagMap.of(..more..)` to add custom fixtures not in scytest
  def all: TagMap[KnownFixture[F, ?]] =
    TagMap.empty

  protected implicit def kfEntry[R](
      kf: KnownFixture[F, R]
  ): TagMap.Entry.Aux[KnownFixture[F, ?], R] = TagMap.Entry(kf.tag, kf)

}

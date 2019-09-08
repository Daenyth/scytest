package scytest.util

import cats.{Id, Monad, ~>}
import cats.implicits._
import scytest.fixture.FixtureTag
import shapeless.{::, HList, HNil}

/** Like an HList but the value at each node is a `FixtureTag.Aux[x]` instead of any shape */
object TagList {

  sealed trait TList {

    /** The underlying `A` types of this TList as an HList, eg `A :: B :: C :: HNil` */
    type H <: HList

    /** The type of this TList as an HList, eg `FixtureTag.Aux[A] :: FixtureTag.Aux[B] :: FixtureTag.Aux[C] :: HNil` */
    type HV <: HList
    def toHList: HV

    /** Extract the underlying `A` values given a natural transformation.
      *
      * `f` is evaluated on the tail element first, moving toward head on each iteration
      */
    def extractRightM[F[_]: Monad](f: FixtureTag.Aux ~> F): F[H]

    /** right monadic fold (starting at the tail) given a function `forall [A], (FixtureTag.Aux[A], B) => F[B]` */
    def foldRightVM[F[_]: Monad, B](init: B)(f: (FixtureTag, B) => F[B]): F[B]

    def foldRight[B](init: B)(f: (FixtureTag, B) => B): B =
      foldRightVM[Id, B](init)(f)

    /** left monadic fold (starting at the head) given a function `forall [A], (B, FixtureTag.Aux[A]) => F[B]` */
    def foldLeftVM[F[_]: Monad, B](init: B)(f: (B, FixtureTag) => F[B]): F[B]

    def foldLeft[B](init: B)(f: (B, FixtureTag) => B): B =
      foldLeftVM[Id, B](init)(f)
  }

  sealed trait TNil extends TList {

    type H = HNil
    type HV = HNil
    val toHList: HV = HNil

    def extractRightM[F[_]: Monad](f: FixtureTag.Aux ~> F): F[HNil] =
      (HNil: HNil).pure[F]

    def :::[H0](h: FixtureTag.Aux[H0]): H0 ::: TNil = TagList.:::(h, this)

    def foldRightVM[F[_]: Monad, B](init: B)(f: (FixtureTag, B) => F[B]): F[B] =
      init.pure[F]

    def foldLeftVM[F[_]: Monad, B](init: B)(f: (B, FixtureTag) => F[B]): F[B] =
      init.pure[F]

  }

  case object TNil extends TNil

  final case class :::[Hd, T <: TList](head: FixtureTag.Aux[Hd], tail: T)
      extends TList {

    def :::[H0](h: FixtureTag.Aux[H0]): H0 ::: Hd ::: T =
      TagList.:::(h, this)

    type H = Hd :: tail.H
    type HV = FixtureTag.Aux[Hd] :: tail.HV
    lazy val toHList: HV = head :: tail.toHList

    def extractRightM[F[_]: Monad](f: FixtureTag.Aux ~> F): F[H] =
      for {
        tl <- tail.extractRightM(f)
        hd <- f(head)
      } yield hd :: tl

    def foldRightVM[F[_]: Monad, B](init: B)(f: (FixtureTag, B) => F[B]): F[B] =
      tail.foldRightVM(init)(f).flatMap(b => f(head, b))

    def foldLeftVM[F[_]: Monad, B](init: B)(f: (B, FixtureTag) => F[B]): F[B] =
      f(init, head).flatMap(b => tail.foldLeftVM(b)(f))

  }

}

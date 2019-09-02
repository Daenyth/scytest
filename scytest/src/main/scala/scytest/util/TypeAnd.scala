package scytest.util

import cats.{Monad, ~>}
import cats.implicits._
import shapeless.{::, HList, HNil}

/** Like an HList but the value at each node is a `V[x]` instead of any shape */
class TypeAnd[V[_]] { TA =>

  /** forall [A], (V[A], B) => F[B] */
  trait RFold[F[_], B] { def apply[A](va: V[A], b: B): F[B] }

  /** forall [A], (B, V[A]) => F[B] */
  trait LFold[F[_], B] { def apply[A](b: B, va: V[A]): F[B] }

  trait Filter { def apply[A](va: V[A]): Boolean }

  sealed trait TList {

    /** The underlying `A` types of this TList as an HList, eg `A :: B :: C :: HNil` */
    type H <: HList

    /** The type of this TList as an HList, eg `V[A] :: V[B] :: V[C] :: HNil` */
    type HV <: HList
    def toHList: HV

    /** Extract the underlying `A` values given a natural transformation.
      *
      * `f` is evaluated on the tail element first, moving toward head on each iteration
      */
    def extractRightM[F[_]: Monad](f: V ~> F): F[H]

    def existentially: List[V[_]]

    /** right monadic fold (starting at the tail) given a function `forall [A], (V[A], B) => F[B]` */
    def foldRightVM[F[_]: Monad, B](init: B)(f: RFold[F, B]): F[B]

    /** left monadic fold (starting at the head) given a function `forall [A], (B, V[A]) => F[B]` */
    def foldLeftVM[F[_]: Monad, B](init: B)(f: LFold[F, B]): F[B]

  }
  // A type tag proving that this `TList` is not `TNil`
  sealed trait NonEmptyTList extends TList

  sealed trait TNil extends TList {

    type H = HNil
    type HV = HNil
    val toHList: HV = HNil

    def extractRightM[F[_]: Monad](f: V ~> F): F[HNil] = (HNil: HNil).pure[F]

    val existentially: List[V[_]] = Nil

    def :::[H0](h: V[H0]): H0 ::: TNil = TA.:::(h, this)

    def foldRightVM[F[_]: Monad, B](init: B)(f: RFold[F, B]): F[B] =
      init.pure[F]

    def foldLeftVM[F[_]: Monad, B](init: B)(f: LFold[F, B]): F[B] =
      init.pure[F]

  }

  case object TNil extends TNil

  final case class :::[Hd, T <: TList](head: V[Hd], tail: T)
      extends TList
      with NonEmptyTList {

    def :::[H0](h: V[H0]): H0 ::: Hd ::: T = TA.:::(h, this)

    type H = Hd :: tail.H
    type HV = V[Hd] :: tail.HV
    lazy val toHList: HV = head :: tail.toHList

    def extractRightM[F[_]: Monad](f: V ~> F): F[H] =
      for {
        tl <- tail.extractRightM(f)
        hd <- f(head)
      } yield hd :: tl

    lazy val existentially: List[V[_]] = head :: tail.existentially

    def foldRightVM[F[_]: Monad, B](init: B)(f: RFold[F, B]): F[B] =
      tail.foldRightVM(init)(f).flatMap(b => f(head, b))

    def foldLeftVM[F[_]: Monad, B](init: B)(f: LFold[F, B]): F[B] =
      f(init, head).flatMap(b => tail.foldLeftVM(b)(f))

  }

}

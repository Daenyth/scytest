package scytest.util

import cats.Monad
import cats.implicits._

/** Like an HList but the value at each node is a `V[x]` instead of any shape */
class TypeAnd[V[_]] { tl =>

  type Cell = TypeAnd.Of[V]
  val Cell = TypeAnd.Of[V]

  /** forall [A], (V[A], B) => F[B] */
  trait RVisitor[F[_], B] {
    def apply[A](va: V[A], b: B): F[B]
  }

  sealed trait TList {
    def existentially: List[V[_]]

    /** Sort of like like a `list.foldRightM`, but with `A` varying at each step */
    def foldRightVM[F[_]: Monad, B](init: B)(f: RVisitor[F, B]): F[B]

  }
  // A type tag proving that this `TList` is not `TNil`
  sealed trait NonEmptyTList extends TList

  sealed trait TNil extends TList {
    val existentially: List[V[_]] = Nil
    def ::[H](h: V[H]): H :: TNil = tl.::(h, this)

    def foldRightVM[F[_]: Monad, B](init: B)(f: RVisitor[F, B]): F[B] =
      init.pure[F]
  }

  case object TNil extends TNil

  final case class ::[H, T <: TList](head: V[H], tail: T)
      extends TList
      with NonEmptyTList {
    lazy val existentially: List[V[_]] = head :: tail.existentially

    def foldRightVM[F[_]: Monad, B](init: B)(f: RVisitor[F, B]): F[B] =
      tail.foldRightVM(init)(f).flatMap(b => f(head, b))
  }

}

object TypeAnd {
  trait Of[V[_]] {
    type T
    def value: V[T]
  }
  object Of {
    type Aux[V[_], T0] = Of[V] { type T = T0 }

    def apply[V[_]] = new Partial[V]
    class Partial[V[_]] {
      def apply[T](value: V[T]): Of.Aux[V, T] = Cell(value)
    }

  }
  case class Cell[V[_], T0](value: V[T0]) extends Of[V] {
    type T = T0
  }
}

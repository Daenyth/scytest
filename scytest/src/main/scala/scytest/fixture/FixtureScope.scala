package scytest.fixture

import cats.kernel.Order
import cats.implicits._

sealed trait FixtureScope

object FixtureScope {
  case object Process extends FixtureScope
  case object Suite extends FixtureScope
  case object Test extends FixtureScope

  implicit val scopeOrder: Order[FixtureScope] = Order.by {
    case Test    => 1
    case Suite   => 2
    case Process => 3
  }

  /** Implicit evidence that Fix2 is "smaller" than Fix1, ie, can be nested inside */
  trait FitsInside[Fix1, Fix2]

  def fits[Fix1, Fix2](
      implicit ev: Fix1 FitsInside Fix2
  ): Fix1 FitsInside Fix2 = ev

  private[this] val singleton: Any FitsInside Any = new FitsInside[Any, Any] {}
  implicit val testInsideClass: Test.type FitsInside Suite.type =
    singleton.asInstanceOf[Test.type FitsInside Suite.type]
  implicit val classInsideProcess: Suite.type FitsInside Process.type =
    singleton.asInstanceOf[Suite.type FitsInside Process.type]

  // Hey look it's a Category
  implicit def insideSelf[A <: FixtureScope]: A FitsInside A =
    singleton.asInstanceOf[A FitsInside A]
  implicit def insideCompose[A, B, C](
      implicit ab: A FitsInside B,
      bc: B FitsInside C
  ): A FitsInside C = {
    val _ = (ab, bc) // unused implicit
    singleton.asInstanceOf[A FitsInside C]
  }
}

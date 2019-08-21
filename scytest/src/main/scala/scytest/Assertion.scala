package scytest

trait Assertion
case object Verified extends Assertion
case class FailedAssertion(cause: Throwable) extends Assertion

package scytest

import cats.MonadError
import scytest.fixture.FixtureTag
import cats.data.NonEmptyVector
import cats.implicits._

import scala.util.control.NonFatal

sealed trait TestResult extends Product with Serializable

object TestResult {
  case class Success(testName: String) extends TestResult
  case class Failed(testName: String, errors: NonEmptyVector[FailedAssertion])
      extends TestResult
  case class Error(testName: String, errors: NonEmptyVector[Throwable])
      extends TestResult
  case class ResourceDead(testName: String, tag: FixtureTag) extends TestResult
  case class Multiple(results: NonEmptyVector[TestResult]) extends TestResult

  def from[F[_]](name: String, test: => F[Assertion])(
      implicit F: MonadError[F, Throwable]
  ): F[TestResult] =
    F.catchNonFatal(test)
      .flatten
      .map {
        case Verified => TestResult.Success(name)
        case f: FailedAssertion =>
          TestResult.Failed(name, NonEmptyVector.one(f))
      }
      .recover {
        case NonFatal(ex) =>
          TestResult.Error(name, NonEmptyVector.one(ex))
      }
}

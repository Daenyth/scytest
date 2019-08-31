package scytest

import scytest.fixture.FixtureTag
import cats.data.NonEmptyVector

sealed trait TestResult extends Product with Serializable

object TestResult {
  case class Success(testName: String) extends TestResult
  case class Failed(testName: String, errors: NonEmptyVector[FailedAssertion])
      extends TestResult
  case class Error(testName: String, errors: NonEmptyVector[Throwable])
      extends TestResult
  case class ResourceDead(testName: String, tag: FixtureTag) extends TestResult
  case class Multiple(results: NonEmptyVector[TestResult]) extends TestResult
}

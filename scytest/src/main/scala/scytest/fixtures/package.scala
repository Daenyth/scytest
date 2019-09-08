package scytest

import java.util.concurrent.Executors

import cats.effect.{Blocker, Resource, Sync}
import scytest.fixture.{Fixture, FixtureScope, FixtureTag}

import scala.concurrent.ExecutionContext

package object fixtures {
  def blocker[F[_]](implicit F: Sync[F]): Fixture[F, Blocker] =
    Fixture
      .resource(
        FixtureTag[Blocker]("cats-blocker", FixtureScope.Process),
        Resource
          .make(F.delay(Executors.newCachedThreadPool())) { e =>
            F.delay(e.shutdown())
          }
          .map { e =>
            Blocker.liftExecutionContext(ExecutionContext.fromExecutor(e))
          }
      )
}

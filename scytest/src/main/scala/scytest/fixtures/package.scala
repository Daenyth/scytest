package scytest

import java.util.concurrent.Executors

import cats.ApplicativeError
import cats.effect.{Blocker, ContextShift, Resource, Sync, Timer}
import scytest.fixture._

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

  def contextShift[F[_]](
      implicit F: ApplicativeError[F, Throwable]
  ): Fixture[F, ContextShift[F]] =
    new ContextShiftFixture[F]

  def timer[F[_]](
      implicit F: ApplicativeError[F, Throwable]
  ): Fixture[F, Timer[F]] =
    new TimerFixture[F]
}

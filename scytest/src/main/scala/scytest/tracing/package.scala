package scytest

import cats.data.Kleisli
import cats.effect.Bracket
import cats.tagless.FunctorK
import com.colisweb.tracing.TracingContext
import fs2.Stream
import cats.tagless.implicits._
import cats.~>

package object tracing {

  implicit class TracerOps[F[_]](val tc: TracingContext[F]) extends AnyVal {
    def childStream[A](
        operationName: String,
        tags: Map[String, String] = Map.empty
    )(
        f: TracingContext[F] => Stream[F, A]
    ): Stream[F, A] =
      Stream.resource(tc.childSpan(operationName, tags)).flatMap(f)
  }

  type TraceT[F[_], B] = Kleisli[F, TracingContext[F], B]
  object TraceT {

    def span[F[_], B](
        operationName: String,
        tags: Map[String, String] = Map.empty
    )(
        fa: TraceT[F, B]
    )(implicit F: Bracket[F, Throwable]): TraceT[F, B] =
      Kleisli { tc =>
        tc.childSpan(operationName, tags).use(tc2 => fa.run(tc2))
      }

    def wrap[F[_], A](
        operationName: String,
        tags: Map[String, String] = Map.empty
    )(
        fa: F[A]
    )(implicit F: Bracket[F, Throwable]): TraceT[F, A] =
      Kleisli { tc =>
        tc.childSpan(operationName, tags).use(_ => fa)
      }

    def runK[Alg[_[_]]: FunctorK, F[_]](
        alg: Alg[TraceT[F, ?]],
        tracer: TracingContext[F]
    ): Alg[F] =
      alg.mapK(Kleisli.applyK(tracer))

    def liftAlgK[Alg[_[_]]: FunctorK, F[_]](
        alg: Alg[F]
    ): Alg[TraceT[F, ?]] =
      alg.mapK(liftK[F])

    def liftK[F[_]]: F ~> TraceT[F, ?] =
      Kleisli.liftK[F, TracingContext[F]]
  }
}

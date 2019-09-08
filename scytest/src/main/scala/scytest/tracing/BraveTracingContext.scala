package scytest.tracing

import cats.data.OptionT
import cats.effect.{Resource, Sync}
import cats.implicits._
import com.colisweb.tracing.TracingContext
import com.colisweb.tracing.TracingContext.TracingContextResource
import zipkin2.Span
import zipkin2.reporter.{AsyncReporter, Sender}

class BraveTracingContext[F[_]](
    root: BraveRootTracer[F],
    span: brave.Span
)(implicit F: Sync[F])
    extends TracingContext[F] {
  def addTags(tags: Map[String, String]): F[Unit] =
    F.delay {
      tags.foreach { case (k, v) => span.tag(k, v) }
    }

  def childSpan(
      operationName: String,
      tags: Map[String, String]
  ): TracingContextResource[F] =
    root.newChild(operationName, tags, parent = span)

  def logError(t: Throwable): F[Unit] =
    F.delay(span.error(t)).void

  override val traceId: OptionT[F, String] =
    OptionT(F.delay(Option(span.context().parentIdString())))

  override val spanId: OptionT[F, String] =
    OptionT.liftF(F.delay(span.context().spanIdString()))
}

class BraveRootTracer[F[_]](tracer: brave.Tracer)(implicit F: Sync[F]) {
  def newTrace(
      operationName: String,
      tags: Map[String, String] = Map.empty
  ): TracingContextResource[F] =
    spanResource(
      F.delay(tracer.newTrace()),
      operationName,
      modify = { span =>
        tags.foreach((span.tag _).tupled)
        span
      }
    )

  def newChild(
      operationName: String,
      tags: Map[String, String],
      parent: brave.Span
  ): TracingContextResource[F] =
    spanResource(
      F.delay(tracer.newChild(parent.context())),
      operationName,
      modify = { span =>
        tags.foreach((span.tag _).tupled)
        span
      }
    )

  private def spanResource(
      make: F[brave.Span],
      operationName: String,
      modify: brave.Span => brave.Span
  ) =
    Resource
      .make(
        make
          .map(_.name(operationName))
          .map(modify)
          .flatMap(s => F.delay(s.start()))
      )(s => F.delay(s.finish()))
      .map(span => new BraveTracingContext[F](this, span): TracingContext[F])
}

object BraveTracingContext {
  sealed trait Reporter
  object Reporter {
    case object Noop extends Reporter
    case object Logging extends Reporter
    case class Zipkin() extends Reporter
  }

  def rootTracer[F[_]](
      reporter: Reporter = Reporter.Logging
  )(
      implicit F: Sync[F]
  ): Resource[F, BraveRootTracer[F]] = {
    def acquire =
      F.delay {
        val b = brave.Tracing
          .newBuilder()
          .localServiceName("scytest")
        reporter match {
          case Reporter.Noop     => b.spanReporter(noopZReporter)
          case Reporter.Logging  => () // do nothing, this is the default
          case Reporter.Zipkin() => b.spanReporter((???): ZReporter)
        }
        b.build()
      }

    Resource
      .make(acquire)(t => F.delay(t.close()))
      .map(_.tracer())
      .map(new BraveRootTracer[F](_))
  }

  type ZReporter = zipkin2.reporter.Reporter[zipkin2.Span]

  private[this] val noopZReporter: ZReporter = (_: Span) => ()

  def asyncReporter[F[_]](
      implicit F: Sync[F]
  ): Resource[F, ZReporter] =
    Resource
      .make(F.delay(AsyncReporter.builder((???): Sender).build()))(
        r => F.delay(r.close())
      )
      .widen[ZReporter]
}

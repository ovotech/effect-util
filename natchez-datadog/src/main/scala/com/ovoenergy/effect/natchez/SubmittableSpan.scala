package com.ovoenergy.effect.natchez

import java.util.concurrent.TimeUnit.NANOSECONDS

import cats.Applicative
import cats.effect.{Clock, ExitCase}
import cats.syntax.apply._
import com.ovoenergy.effect.natchez.DatadogTags.{forThrowable, SpanType}
import io.circe.{Decoder, Encoder}
import io.circe.Encoder.encodeString
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto._
import natchez.TraceValue
import natchez.TraceValue.StringValue

/**
 * This is the type we need to send to the Datadog agent
 * to submit traces. Turning a span into one of these involves some trickery
 * with tags, hence this being its own file now
 */
case class SubmittableSpan(
  traceId: Long,
  spanId: Long,
  name: String,
  service: String,
  resource: String,
  `type`: Option[SpanType],
  start: Long,
  duration: Long,
  parentId: Option[Long],
  error: Option[Int],
  meta: Map[String, String],
  metrics: Map[String, Double]
)

object SubmittableSpan {

  implicit val config: Configuration =
    Configuration.default.withSnakeCaseMemberNames

  implicit val encodeSpanType: Encoder[SpanType] =
    encodeString.contramap(_.toString.toLowerCase)

  implicit val decodespanType: Decoder[Option[SpanType]] =
    Decoder[Option[String]].map(_.flatMap(s => inferSpanType(Map("span.type" -> s))))

  implicit val encode: Encoder[SubmittableSpan] =
    deriveConfiguredEncoder

  implicit val decode: Decoder[SubmittableSpan] =
    deriveConfiguredDecoder

  /**
   * It is very difficult to find any docs on this other than this fun Github issue by fommil
   * https://github.com/DataDog/datadog-agent/issues/3031 but setting this magic metric to 2
   * means the Datadog Agent should always keep all our traces.
   */
  private val spanMetrics: Map[String, Double] =
    Map("__sampling_priority_v1" -> 2.0d)

  /**
   * Datadog docs: "Set this [error] value to 1 to indicate if an error occurred"
   */
  private def isError[A](exitCase: ExitCase[A]): Option[Int] =
    exitCase match {
      case ExitCase.Completed => None
      case ExitCase.Error(_)  => Some(1)
      case ExitCase.Canceled  => None
    }

  /**
   * Create some Datadog tags from an exit case,
   * i.e. if the span failed include exception details
   */
  private def exitTags(exitCase: ExitCase[Throwable]): Map[String, String] =
    exitCase match {
      case ExitCase.Error(e) => forThrowable(e).view.mapValues(_.value.toString).toMap
      case _                 => Map.empty
    }

  /**
   * Sadly the only API we have to set span type is through tags
   * so we just have to guess if the user tried to set it
   */
  private def inferSpanType(tags: Map[String, TraceValue]): Option[SpanType] =
    tags.collectFirst {
      case ("span.type", StringValue("cache")) => SpanType.Cache
      case ("span.type", StringValue("web"))   => SpanType.Web
      case ("span.type", StringValue("db"))    => SpanType.Db
    }

  /**
   * Turn the Natchez tags into DataDog metadata.
   * We filter out the magic span.type tag and add a trace token + error info
   */
  private def transformTags(
    tags: Map[String, TraceValue],
    exitCase: ExitCase[Throwable],
    traceToken: String
  ): Map[String, String] =
    exitTags(exitCase) ++
    tags.view
      .mapValues(_.value.toString)
      .filterKeys(_ != "span.type")
      .toMap
      .updated("traceToken", traceToken)

  /**
   * Given an open DatadogSpan and an exit case to indicate whether the span succeeded
   * Create a submittable span we can pass through to the DataDog agent API
   */
  def fromSpan[F[_]: Applicative: Clock](
    span: DatadogSpan[F],
    exitCase: ExitCase[Throwable]
  ): F[SubmittableSpan] =
    (
      span.meta.get,
      span.ids.get,
      Clock[F].realTime(NANOSECONDS)
    ).mapN {
      case (meta, ids, end) =>
        SubmittableSpan(
          traceId = ids.traceId,
          spanId = ids.spanId,
          name = span.names.name,
          service = span.names.service,
          resource = span.names.resource,
          start = span.start,
          duration = end - span.start,
          parentId = ids.parentId,
          error = isError(exitCase),
          `type` = inferSpanType(meta),
          metrics = spanMetrics,
          meta = transformTags(meta, exitCase, ids.traceToken)
        )
    }
}

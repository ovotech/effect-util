package com.ovoenergy.natchez.extras.datadog

import cats.effect.Sync
import cats.syntax.apply._
import cats.syntax.functor._
import com.ovoenergy.natchez.extras.datadog.data.UnsignedLong
import com.ovoenergy.natchez.extras.datadog.headers.TraceHeaders._
import natchez.Kernel
import org.http4s.Headers
import org.typelevel.ci._

import java.util.UUID

case class SpanIdentifiers(
  traceId: UnsignedLong,
  spanId: UnsignedLong,
  parentId: Option[UnsignedLong],
  traceToken: String
)

object SpanIdentifiers {

  private def randomUUID[F[_]: Sync]: F[String] =
    Sync[F].delay(UUID.randomUUID.toString)

  def create[F[_]: Sync]: F[SpanIdentifiers] =
    (
      UnsignedLong.random[F],
      UnsignedLong.random[F],
      Sync[F].pure(None),
      randomUUID
    ).mapN(SpanIdentifiers.apply)

  /**
   * Create span identifiers that identify a new child span of the one identified by the provided ids
   * This means the parent ID will be set but a new span ID will be created
   */
  def child[F[_]: Sync](identifiers: SpanIdentifiers): F[SpanIdentifiers] =
    UnsignedLong.random.map(spanId => identifiers.copy(parentId = Some(identifiers.spanId), spanId = spanId))

  private def orRandom[F[_]: Sync](option: Option[UnsignedLong]): F[UnsignedLong] =
    option.fold(UnsignedLong.random)(Sync[F].pure)

  private def traceId[F[_]: Sync](headers: Headers): F[UnsignedLong] =
    orRandom(headers.get[`X-Trace-Id`].map(_.value).orElse(headers.get[`X-B3-Trace-Id`].map(_.value)))

  private def parentId(headers: Headers): Option[UnsignedLong] =
    headers.get[`X-Parent-Id`].map(_.value).orElse(headers.get[`X-B3-Span-Id`].map(_.value))

  /**
   * Build span identifiers from HTTP headers provided by a client,
   * this will always succeed even if headers are missing because
   * partial data (i.e. just a trace token) is still useful to us
   */
  def fromKernel[F[_]: Sync](rawKernel: Kernel): F[SpanIdentifiers] = {
    val headers = Headers(rawKernel.toHeaders.toSeq)
    (
      traceId(headers),
      UnsignedLong.random[F],
      Sync[F].pure(parentId(headers)),
      headers.get(ci"x-trace-token").fold(randomUUID)(v => Sync[F].pure(v.head.value))
    ).mapN(SpanIdentifiers.apply)
  }

  def toKernel(ids: SpanIdentifiers): Kernel =
    Kernel(
      Headers(
        `X-Trace-Id`(ids.traceId),
        `X-Parent-Id`(ids.spanId),
        `X-B3-Trace-Id`(ids.traceId),
        `X-B3-Span-Id`(ids.spanId),
        "X-Trace-Token" -> ids.traceToken
      ).headers.map(r => r.name.toString -> r.value).toMap
    )
}

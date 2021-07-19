package com.ovoenergy.natchez.extras.datadog.headers

import cats.syntax.invariant._
import HeaderInstances._
import com.ovoenergy.natchez.extras.datadog.data.UnsignedLong
import org.typelevel.ci._

/**
 * Here we explicitly model each of the headers we use to propagate traces,
 * just to make it clearer exactly what we expect format-wise
 */
object TraceHeaders {

  case class `X-Span-Id`(value: UnsignedLong)

  object `X-Span-Id` {
    implicit val header: SingleHeader[`X-Span-Id`] =
      unsignedLong(ci"X-Span-Id", radix = 10).imap(`X-Span-Id`.apply)(_.value)
  }

  case class `X-Trace-Id`(value: UnsignedLong)

  object `X-Trace-Id` {
    implicit val header: SingleHeader[`X-Trace-Id`] =
      unsignedLong(ci"X-Trace-Id", radix = 10).imap(`X-Trace-Id`.apply)(_.value)
  }

  case class `X-Parent-Id`(value: UnsignedLong)

  object `X-Parent-Id` {
    implicit val header: SingleHeader[`X-Parent-Id`] =
      unsignedLong(ci"X-Parent-Id", radix = 10).imap(`X-Parent-Id`.apply)(_.value)
  }

  case class `X-B3-Span-Id`(value: UnsignedLong)

  object `X-B3-Span-Id` {
    implicit val header: SingleHeader[`X-B3-Span-Id`] =
      unsignedLong(ci"X-B3-Span-Id", radix = 16).imap(`X-B3-Span-Id`.apply)(_.value).transform(_.take(16))
  }

  case class `X-B3-Trace-Id`(value: UnsignedLong)

  object `X-B3-Trace-Id` {
    implicit val header: SingleHeader[`X-B3-Trace-Id`] =
      unsignedLong(ci"X-B3-Trace-Id", radix = 16).imap(`X-B3-Trace-Id`.apply)(_.value).transform(_.take(16))
  }
}

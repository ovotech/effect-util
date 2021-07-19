package com.ovoenergy.natchez.extras.datadog.headers

import cats.Invariant
import cats.syntax.either._
import com.ovoenergy.natchez.extras.datadog.data.UnsignedLong
import org.http4s.Header.Single
import org.http4s.{Header, ParseFailure}
import org.typelevel.ci.CIString

/**
 * Some additional instances for HTTP4s headers
 * to make it easier to decode the propagation headers we need
 */
object HeaderInstances {

  type SingleHeader[A] = Header[A, Single]

  /**
   * Parse a header value from a string into an unsigned long,
   * The radix allows both decimal and hex encoded longs to be parsed
   */
  def unsignedLong(headerName: CIString, radix: Int): SingleHeader[UnsignedLong] =
    new SingleHeader[UnsignedLong] {
      def name: CIString =
        headerName
      def value(a: UnsignedLong): String =
        a.toString(radix)
      def parse(headerValue: String): Either[ParseFailure, UnsignedLong] =
        UnsignedLong.fromString(headerValue, radix).leftMap { _ =>
          ParseFailure("", s"Failed to parse $headerName as unsigned long")
        }
    }

  /**
   * Allow the string representation of a header to be transformed
   * prior to it being passed to / after being recieved from the underlying typed header
   */
  implicit class HeaderOps[A](header: SingleHeader[A]) {
    def transform(f: String => String): SingleHeader[A] =
      new SingleHeader[A] {
        def name: CIString = header.name
        def value(a: A): String = f(header.value(a))
        def parse(headerValue: String): Either[ParseFailure, A] = header.parse(f(headerValue))
      }
  }

  /**
   * Make it easy to create a header of a specific newtype
   * by mapping to and from the underlying header value
   */
  implicit val invariant: Invariant[SingleHeader] =
    new Invariant[SingleHeader] {
      def imap[A, B](fa: SingleHeader[A])(f: A => B)(g: B => A): SingleHeader[B] =
        new SingleHeader[B] {
          def name: CIString = fa.name
          def value(a: B): String = fa.value(g(a))
          def parse(headerValue: String): Either[ParseFailure, B] = fa.parse(headerValue).map(f)
        }
    }
}

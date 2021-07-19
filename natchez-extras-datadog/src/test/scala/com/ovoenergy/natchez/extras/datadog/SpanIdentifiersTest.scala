package com.ovoenergy.natchez.extras.datadog

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.ovoenergy.natchez.extras.datadog.SpanIdentifiers._
import com.ovoenergy.natchez.extras.datadog.data.UnsignedLong
import natchez.Kernel
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{EitherValues, OptionValues}
import org.scalatestplus.scalacheck.Checkers

class SpanIdentifiersTest
    extends AnyWordSpec
    with Matchers
    with Checkers
    with OptionValues
    with EitherValues {

  "Span identifiers" should {

    "Set IDs correctly when creating child IDs" in {

      val (parent, child) = (
        for {
          parent <- SpanIdentifiers.create[IO]
          child  <- SpanIdentifiers.child[IO](parent)
        } yield parent -> child
      ).unsafeRunSync()

      parent.parentId shouldBe None
      child.traceId shouldBe parent.traceId
      child.parentId shouldBe Some(parent.spanId)
      child.spanId should not be parent.spanId
    }

    "Convert to and from a kernel losslessly" in {
      val (original, kernel) = (
        for {
          ids    <- create[IO]
          kernel <- fromKernel[IO](SpanIdentifiers.toKernel(ids))
        } yield ids -> kernel
      ).unsafeRunSync()

      kernel.traceId shouldBe original.traceId
      kernel.traceToken shouldBe original.traceToken
      kernel.parentId shouldBe Some(original.spanId)
    }
  }

  "Succeed in converting from a kernel even if info is missing" in {
    fromKernel[IO](Kernel(Map.empty)).attempt.unsafeRunSync() should matchPattern { case Right(_) => }
    fromKernel[IO](Kernel(Map("X-Trace-Token" -> "foo"))).unsafeRunSync().traceToken shouldBe "foo"
  }

  "Ignore header case when extracting info" in {
    fromKernel[IO](Kernel(Map("x-TRACe-tokeN" -> "foo"))).unsafeRunSync().traceToken shouldBe "foo"
  }

  "Output hex-encoded B3 Trace IDs alongside decimal encoded Datadog IDs" in {
    val result = SpanIdentifiers.create[IO].map(SpanIdentifiers.toKernel).unsafeRunSync()
    val ddSpanId: String = result.toHeaders.get("X-Trace-Id").value
    val b3SpanId: String = result.toHeaders.get("X-B3-Trace-Id").value
    val ddULong = UnsignedLong.fromString(ddSpanId, 10)
    val b3ULong = UnsignedLong.fromString(b3SpanId, 16)
    ddULong shouldBe b3ULong
  }

  "Output hex-encoded B3 Span IDs alongside decimal encoded Datadog Parent IDs" in {
    val result = SpanIdentifiers.create[IO].map(SpanIdentifiers.toKernel).unsafeRunSync()
    val ddSpanId: String = result.toHeaders.get("X-Parent-Id").value
    val b3SpanId: String = result.toHeaders.get("X-B3-Span-Id").value
    val ddULong = UnsignedLong.fromString(ddSpanId, 10)
    val b3ULong = UnsignedLong.fromString(b3SpanId, 16)
    ddULong shouldBe b3ULong
  }
}

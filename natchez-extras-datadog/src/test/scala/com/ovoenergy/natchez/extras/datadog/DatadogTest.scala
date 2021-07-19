package com.ovoenergy.natchez.extras.datadog

import cats.effect._
import cats.effect.unsafe.implicits._
import cats.instances.list._
import cats.syntax.traverse._
import com.ovoenergy.natchez.extras.datadog.Datadog.entryPoint
import com.ovoenergy.natchez.extras.datadog.DatadogTags.SpanType.{Cache, Db, Web}
import com.ovoenergy.natchez.extras.datadog.DatadogTags.spanType
import natchez.EntryPoint
import org.http4s.Request
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.syntax.literals._
import org.scalatest.{Inspectors, OptionValues}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration._

/**
 * This tests both the datadog span code itself and the submission of metrics over HTTP
 * Could be expanded but even just these tests exposed concurrency issues with my original code.
 */
class DatadogTest extends AnyWordSpec with Matchers with OptionValues {

  def run(f: EntryPoint[IO] => IO[Unit]): IO[List[Request[IO]]] =
    TestClient[IO].flatMap(c => entryPoint(c.client, "test", "blah").use(f) >> c.requests)

  "Datadog span" should {

    "Obtain the agent host from the parameter" in {
      (
        for {
          client <- TestClient[IO]
          ep = entryPoint(client.client, "a", "b", agentHost = uri"http://example.com")
          _        <- ep.use(_.root("foo").use(_ => IO.unit))
          requests <- client.requests
        } yield requests.map(_.uri) shouldBe List(
          uri"http://example.com/v0.3/traces"
        )
      ).unsafeRunSync()
    }

    "Allow you to modify trace tokens" in {
      (
        for {
          client <- TestClient[IO]
          ep = entryPoint(client.client, "a", "b", agentHost = uri"http://example.com")
          kernel <- ep.use(_.root("foo").use(s => s.put("traceToken" -> "foo") >> s.kernel))
        } yield kernel.toHeaders.get("X-Trace-Token") shouldBe Some("foo")
      ).unsafeRunSync()
    }

    "Continue to send HTTP calls even if one of them fails" in {

      val test: EntryPoint[IO] => IO[Unit] =
        ep =>
          ep.root("first").use(_ => IO.unit) >>
          IO.sleep(1.second) >>
          ep.root("second").use(_ => IO.unit)
      (
        for {
          client <- TestClient[IO]
          _      <- client.respondWith(IO.raiseError(new Exception))
          ep = entryPoint(client.client, "a", "b", agentHost = uri"http://example.com")
          _        <- ep.use(test)
          requests <- client.requests
        } yield requests.length shouldBe 2
      ).unsafeRunSync()
    }

    "Submit the right info to Datadog when closed" in {

      val res = run(_.root("bar:res").use(_.put("k" -> "v") >> IO.sleep(1.milli))).unsafeRunSync()
      val span = res.flatTraverse(_.as[List[List[SubmittableSpan]]]).unsafeRunSync().flatten.head

      span.name shouldBe "bar"
      span.service shouldBe "test"
      span.resource shouldBe "res"
      span.`type` shouldBe None
      span.duration > 0 shouldBe true
      span.meta.get("k") shouldBe Some("v")
      span.meta.contains("traceToken") shouldBe true
    }

    "Only include the sampling priority metric on the root span" in {
      val res = run(_.root("root").use(_.span("span").use(_ => IO.sleep(1.milli)))).unsafeRunSync()
      val spans = res.flatTraverse(_.as[List[List[SubmittableSpan]]]).unsafeRunSync().flatten
      spans.find(_.parentId.isEmpty).value.metrics shouldBe Map("_sampling_priority_v1" -> 2.0)
      spans.find(_.parentId.isDefined).value.metrics shouldBe Map.empty
    }

    "Infer the right span.type from any tags set" in {
      Inspectors.forAll(List(Web, Cache, Db)) { typ =>
        val res = run(_.root("bar:res").use(_.put(spanType(typ)))).unsafeRunSync()
        val span = res.flatTraverse(_.as[List[List[SubmittableSpan]]]).unsafeRunSync().flatten.head
        span.`type` shouldBe Some(typ)
      }
    }

    "Submit multiple spans across multiple calls when span() is called" in {
      val res = run(_.root("bar").use(_.span("subspan").use(_ => IO.sleep(1.second)))).unsafeRunSync()
      val spans = res.flatTraverse(_.as[List[List[SubmittableSpan]]]).unsafeRunSync().flatten
      spans.map(_.traceId).distinct.length shouldBe 1
      spans.map(_.spanId).distinct.length shouldBe 2
    }

    "Allow you to override the service name and resource with colons" in {
      val res = run(_.root("svc:name:res").use(_ => IO.unit)).unsafeRunSync()
      val spans = res.flatTraverse(_.as[List[List[SubmittableSpan]]]).unsafeRunSync().flatten
      spans.head.resource shouldBe "res"
      spans.head.service shouldBe "svc"
      spans.head.name shouldBe "name"
    }

    "Inherit metadata into subspans but only at the time of creation" in {
      val res = run(
        _.root("bar:res").use { root =>
          root.put("foo" -> "bar") >> root.span("sub").use(_.put("baz" -> "qux"))
        }
      ).unsafeRunSync()

      val spans = res.flatTraverse(_.as[List[List[SubmittableSpan]]]).unsafeRunSync().flatten
      val rootSpan = spans.find(_.name == "bar").get
      val subSpan = spans.find(_.name == "sub").get

      subSpan.meta.view.filterKeys(_ != "traceToken").toMap shouldBe Map("foo" -> "bar", "baz" -> "qux")
      rootSpan.meta.view.filterKeys(_ != "traceToken").toMap shouldBe Map("foo" -> "bar")
    }
  }
}

---
layout: docs
title: "Natchez Doobie"
---


# Natchez Doobie

`natchez-doobie` provides a `Transactor` that adds spans for database queries to your traces.

## Installation

```scala
val effectUtilsVersion = "@VERSION@"
resolvers += Resolver.bintrayRepo("ovotech", "maven")

libraryDependencies ++= Seq(
  "com.ovoenergy.effect" %% "natchez-doobie" % effectUtilsVersion
)
```

## Usage

If you want to run this example yourself you can use Docker to spin up a temporary Postgres instance:
```bash
docker run -d -p5432:5432 -e"POSTGRES_PASSWORD=password" -e"POSTGRES_USER=postgres" postgres
```

This example demonstrates connecting to a database with Doobie, wrapping the transactor into a `TracedTransactor`
and then passing that into a tagless final application that queries the database.

```scala mdoc
import cats.data.Kleisli
import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource, Sync}
import cats.syntax.functor._
import com.ovoenergy.effect.natchez.{Datadog, TracedTransactor}
import doobie.implicits._
import doobie.util.transactor.Transactor
import natchez.{EntryPoint, Span, Trace}
import org.http4s.client.blaze.BlazeClientBuilder

import scala.concurrent.ExecutionContext.global

object NatchezDoobie extends IOApp {

  type TracedIO[A] = Kleisli[IO, Span[IO], A]

  /**
   * Create a Natchez entrypoint that will send traces to Datadog
   */
  val datadog: Resource[IO, EntryPoint[IO]] =
    for {
      httpClient <- BlazeClientBuilder[IO](global).withDefaultSslContext.resource
      entryPoint <- Datadog.entryPoint(httpClient, "example-database", "default-resource")
    } yield entryPoint

  /**
   * Create a Doobie transactor that connects to a preexisting postgres instance
   * and then wrap it in TracedTransactor so it creates spans for queries
   */
  val transactor: Resource[IO, Transactor[TracedIO]] =
    Blocker[IO].map { blocker =>
      TracedTransactor(
        service = "my-example-service-db",
        blocking = blocker,
        transactor = Transactor.fromDriverManager[IO](
          driver = "org.postgresql.Driver",
          url = "jdbc:postgresql:example",
          user = "postgres",
          pass = "password" // of course don't hard code these details in your applications!
        )
      )
    }

  /**
   * Your application code doesn't need to know about the TracedIO type,
   * it just works with an effect type that has a Trace instance
   */
  def application[F[_]: Sync: Trace](db: Transactor[F]): F[ExitCode] =
    sql"SELECT * FROM example"
      .query[String]
      .to[List]
      .transact(db)
      .map(println)
      .as(ExitCode.Success)

  /**
   * To run the application we create a root span
   * and use that to turn the application from a TracedIO into an IO
   */
   def run(args: List[String]): IO[ExitCode] =
     datadog.use { entryPoint =>
       entryPoint.root("root_span").use { root =>
         transactor.use { db => application(db).run(root) }
       }
     }
}
```

Upon running this code you should see a trace like this show up in Datadog.
Note that `-db` is automatically appended to the service name you provide `TracedTransactor`.

![datadog trace]({{site.baseurl}}/img/example-doobie-trace.png)

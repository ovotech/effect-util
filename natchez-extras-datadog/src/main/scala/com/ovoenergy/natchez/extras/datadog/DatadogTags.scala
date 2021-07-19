package com.ovoenergy.natchez.extras.datadog

import natchez.TraceValue

/**
 * Some helper functions to construct Datadog tags,
 * I took these from the DD-Trace-Java library
 */
object DatadogTags {

  sealed trait SpanType

  object SpanType {
    case object Custom extends SpanType
    case object Cache extends SpanType
    case object Web extends SpanType
    case object Db extends SpanType
  }

  def env(env: String): (String, TraceValue) =
    "env" -> env

  def spanType(spanType: SpanType): (String, TraceValue) =
    "span.type" -> spanType.toString.toLowerCase

  def serviceName(name: String): (String, TraceValue) =
    "service.name" -> name

  def sqlQuery(query: String): (String, TraceValue) =
    "sql.query" -> query

  def httpMethod(method: String): (String, TraceValue) =
    "http.method" -> method

  def httpStatusCode(code: Int): (String, TraceValue) =
    "http.status_code" -> code

  def httpUrl(url: String): (String, TraceValue) =
    "http.url" -> url

  def errorMessage(what: String): (String, TraceValue) =
    "error.msg" -> what

  def errorType(what: String): (String, TraceValue) =
    "error.type" -> what

  def errorStack(what: String): (String, TraceValue) =
    "error.stack" -> what

  def forThrowable(e: Throwable): Map[String, TraceValue] =
    (
      Option(e.getMessage).map(errorMessage).toList ++
      List(errorType(e.getClass.getSimpleName), errorStack(e.getStackTrace.mkString("\n")))
    ).toMap
}

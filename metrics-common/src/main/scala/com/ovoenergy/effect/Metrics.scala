package com.ovoenergy.effect

import cats.{Functor, Monad, ~>}
import cats.syntax.apply._
import com.ovoenergy.effect.Metrics.Metric

/**
 * A type class representing the ability to push metrics
 * within some effect type F
 */
trait Metrics[F[_]] {
  def counter(metric: Metric)(value: Long): F[Unit]
  def histogram(metric: Metric)(value: Long): F[Unit]
}

object Metrics {

  case class Metric(name: String, tags: Map[String, String])
  def apply[F[_]: Metrics]: Metrics[F] = implicitly

  /**
   * Transform the effect type of the given Metrics instance with a natural transformation
   */
  def mapK[F[_], G[_]: Functor](metrics: Metrics[F], nt: F ~> G): Metrics[G] =
    new Metrics[G] {
      def counter(metric: Metric)(value: Long): G[Unit] =
        nt(metrics.counter(metric)(value))
      def histogram(metric: Metric)(value: Long): G[Unit] =
        nt(metrics.histogram(metric)(value))
    }

  /**
   * Combine instances to publish to two different metric providers sequentially,
   * useful when migrating between metrics providers
   */
  def combine[F[_]: Monad](a: Metrics[F], b: Metrics[F]): Metrics[F] =
    new Metrics[F] {
      def counter(m: Metric)(value: Long): F[Unit] =
        (a.counter(m)(value), b.counter(m)(value)).mapN {
          case ((), ()) => ()
        }
      def histogram(m: Metric)(value: Long): F[Unit] =
        (a.histogram(m)(value), b.histogram(m)(value)).mapN {
          case ((), ()) => ()
        }
    }
}

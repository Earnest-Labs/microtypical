package com.earnest.microtypical.syntax.cats

import cats.effect.syntax.effect._
import cats.effect.{ContextShift, Effect, Timer}
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.earnest.microtypical.syntax.scala.future._
import com.twitter.util.{Future => TwitterFuture}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions

final class EffectOps [F[_], A] (self: F [A]) (implicit F: Effect [F]) {
  def retryWithBackoff(initialDelay: FiniteDuration, maxRetries: Int)(implicit timer: Timer [F]): F [A] =
    F.handleErrorWith (self) { error =>
      if (maxRetries > 0)
        timer.sleep(initialDelay) >> retryWithBackoff(initialDelay * 2, maxRetries - 1)
      else
        F.raiseError(error)
    }

  /**
    * @param ec ExecutionContext to use while executing this effect
    * @param cs ContextShift
    * @return
    */
  def fork(ec: ExecutionContext)(implicit cs: ContextShift [F]): F [A] = cs.evalOn(ec)(self)

  /**
    * @param ec ExecutionContext to use while executing this effect
    * @param cs ContextShift
    * @return
    */
  def forkMap [B] (f: A => B)(ec: ExecutionContext)(implicit cs: ContextShift[F]): F [B] = fork(ec).map(f)

  def toTwitter(fec: ExecutionContext): TwitterFuture [A] =
    self.toIO.unsafeToFuture().toTwitter(fec)
}

trait ToEffectOps {
  implicit def toEffOps [F[_]: Effect, A] (self: F [A]): EffectOps [F, A] =
    new EffectOps(self)
}

object effect extends ToEffectOps

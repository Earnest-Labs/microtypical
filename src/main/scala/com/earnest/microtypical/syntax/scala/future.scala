package com.earnest.microtypical.syntax.scala

import scala.concurrent.{ExecutionContext, Future}
import com.twitter.util.{Future => TFuture, Promise => TPromise}
import com.earnest.microtypical.syntax.scala.try_._
import scala.language.implicitConversions

final class ScalaFutureOps[A](val self: Future [A]) extends AnyVal {
  def toTwitter (implicit ec: ExecutionContext): TFuture [A] = {
    val promise = TPromise [A] ()
    self.onComplete(promise.update _ compose (_.toTwitter)) (ec)
    promise
  }
}

trait ToScalaFutureOps {
  implicit def toFutureOps [A] (self: Future [A]): ScalaFutureOps [A] =
    new ScalaFutureOps[A] (self)
}

object future extends ToScalaFutureOps

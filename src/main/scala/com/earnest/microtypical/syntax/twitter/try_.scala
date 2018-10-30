package com.earnest.microtypical.syntax.twitter

import com.twitter.util.{Return, Throw, Try}
import scala.util.{Success, Try => STry, Failure}
import scala.language.implicitConversions

final class TwitterTryOps [A](val self: Try [A]) extends AnyVal {
  def fold [B] (fe: Throwable => B, fa: A => B): B = self match {
    case Throw (e) => fe (e)
    case Return (a) => fa (a)
  }

  def toScala : STry [A] = fold [STry [A]] (Failure.apply, Success.apply)
}

trait ToTwitterTryOps {
  implicit def toTryOps [A](self: Try [A]): TwitterTryOps [A] =
    new TwitterTryOps [A] (self)
}

object try_ extends ToTwitterTryOps

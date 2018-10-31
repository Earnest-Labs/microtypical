package com.earnest.microtypical.syntax.scala

import com.twitter.util.{Return, Throw, Try => TTry}

import scala.util.{Failure, Success, Try}
import scala.language.implicitConversions

final class ScalaTryOps [A](val self: Try [A]) extends AnyVal {
  def toTwitter : TTry [A] = fold [TTry [A]] (Throw.apply, Return.apply)

  def fold [B] (fe: Throwable => B, fa: A => B): B = self match {
    case Success (a) => fa (a)
    case Failure (e) => fe (e)
  }
}

trait ToScalaTryOps {
  implicit def toScalaTryOps [A] (self: Try [A]): ScalaTryOps [A] =
    new ScalaTryOps (self)
}

object try_ extends ToScalaTryOps

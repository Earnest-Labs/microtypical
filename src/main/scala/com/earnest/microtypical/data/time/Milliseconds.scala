package com.earnest.microtypical.data.time

import com.earnest.microtypical.data.implicits.microtypicalDataArbitraryFromIso
import com.earnest.microtypical.data.numeric.Gteq0
import com.earnest.microtypical.data.{Iso, Validated}
import org.scalacheck.Arbitrary

case class Milliseconds (value: Gteq0 [Int]) extends Validated

object Milliseconds {
  implicit val iso: Iso [Milliseconds, Gteq0 [Int]] = Iso .instance (_.value, apply)

  implicit def arbitrary: Arbitrary [Milliseconds] = microtypicalDataArbitraryFromIso

  val maxValue: Milliseconds = apply (Gteq0 applyUnsafe Int.MaxValue)

  def time [A] (a: => A): (A, Milliseconds) = {
    val t0 = System.nanoTime
    val value: A = a
    val dt = (System.nanoTime - t0) / 1000000
    (value, Gteq0 preview dt.toInt map apply getOrElse maxValue)
  }
}

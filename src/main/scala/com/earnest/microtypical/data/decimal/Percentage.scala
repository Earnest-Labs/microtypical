package com.earnest.microtypical.data.decimal

import cats.Show
import cats.instances.int.catsStdShowForInt
import com.earnest.microtypical.data.Validated
import com.earnest.microtypical.data.validation.BoundedCompanion
import org.scalacheck.{Arbitrary, Gen}

abstract sealed case class Percentage (value: Int) extends Validated

object Percentage extends BoundedCompanion.Inclusive [Percentage, Int] {
  override def min = 0
  override def max = 100

  override def ordering: Ordering [Int] = implicitly
  override def show: Show [Int] = implicitly

  override protected [this] def wrap (i: Int): Percentage = new Percentage (i) {}
  override protected [this] def unwrap (v: Percentage): Int = v.value

  implicit def arbitrary: Arbitrary [Percentage] = Arbitrary (Gen choose (min, max) map applyUnsafe)
}

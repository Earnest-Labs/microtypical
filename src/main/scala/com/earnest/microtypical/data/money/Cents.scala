package com.earnest.microtypical.data.money

import cats.Show
import cats.instances.long.catsStdShowForLong
import com.earnest.microtypical.data.Validated
import com.earnest.microtypical.data.validation.BoundedCompanion

abstract sealed case class Cents (value: Long) extends Validated

object Cents extends BoundedCompanion.Inclusive [Cents, Long] {
  override protected [this] def wrap (r: Long): Cents = new Cents (r) {}
  override protected [this] def unwrap (v: Cents): Long = v.value
  override def ordering: Ordering [Long] = implicitly
  override def show: Show [Long] = implicitly

  override def min: Long = minCents
  override def max: Long = maxCents
}

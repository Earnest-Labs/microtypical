package com.earnest.microtypical.data.money

import com.earnest.microtypical.Show
import com.earnest.microtypical.data.validation.{BoundedCompanion, Errors}
import com.earnest.microtypical.data.{Model, Validated}

abstract sealed case class CentsGteq0 (value: Long) extends Validated

object CentsGteq0 extends BoundedCompanion.Inclusive [CentsGteq0, Long] {
  override protected [this] def wrap (r: Long): CentsGteq0 = new CentsGteq0 (r) {}
  override protected [this] def unwrap (v: CentsGteq0): Long = v.value
  override def ordering: Ordering [Long] = implicitly
  override def show: Show [Long] = implicitly

  override def min: Long = 0L
  override def max: Long = maxCents

  val fromCents: Model [CentsGteq0, Cents, Errors] = Model subtype (model, Cents.model)
}

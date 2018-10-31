package com.earnest.microtypical.data.validation

import com.earnest.microtypical.Show
import cats.syntax.functor._
import com.earnest.microtypical.data.{Bounds, GapFree, Model, Representation, ValidatedCompanion}

trait BoundedCompanion [V, R] extends ValidatedCompanion [V, R, Errors] {
  protected [this] def wrap (r: R): V
  protected [this] def unwrap (v: V): R
  def bounds: Bounds [R]
  def ordering: Ordering [R]
  def show: Show [R]

  final implicit def gapFree: GapFree [V] = GapFree.instance

  final implicit def boundsR: Bounds [Representation [V, R]] = bounds map Representation.apply

  final implicit def orderingR: Ordering [Representation [V, R]] = Ordering .by [Representation [V, R], R] (_.value) (ordering)

  final def constraints: Constraints [R] = bounded (bounds) (ordering, show)

  final override implicit val model: Model [V, R, Errors] = Model instance (validate (constraints, wrap), unwrap)
}

object BoundedCompanion {
  trait Inclusive [V, R] extends BoundedCompanion [V, R] {
    def min: R
    def max: R

    final def minValue: V = applyUnsafe (min)
    final def maxValue: V = applyUnsafe (max)

    final override def bounds: Bounds [R] = Bounds inclusive (min, max)

    final def withinBounds (r: R): V = r match {
      case _ if ordering lteq (r, min) => minValue
      case _ if ordering gteq (r, max) => maxValue
      case _ => applyUnsafe (r)
    }
  }
}

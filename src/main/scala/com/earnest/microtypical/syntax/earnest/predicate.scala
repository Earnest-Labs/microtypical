package com.earnest.microtypical.syntax.earnest

import com.earnest.microtypical.Predicate

import scala.language.implicitConversions

final class PredicateOps [A] (val self : Predicate [A]) extends AnyVal {
  def not: Predicate [A] = a => ! self (a)
  def `unary_!`: Predicate [A] = a => ! self (a)
}

trait ToPredicateOps {
  implicit def toPredicateOps [A, B <: Boolean] (self: A => B): PredicateOps [A] =
    new PredicateOps [A] (self)
}

object predicate extends ToPredicateOps

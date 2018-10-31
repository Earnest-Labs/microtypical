package com.earnest.microtypical.syntax.earnest

import com.earnest.microtypical.Predicate
import scala.language.implicitConversions

final class PredicateOps [A] (val self : Predicate [A]) extends AnyVal {
  def not: Predicate [A] = a => ! self (a)
}

trait ToPredicateOps {
  implicit def PredicateOps [A](self: Predicate [A]): PredicateOps [A] =
    new PredicateOps [A] (self)
}

object predicate extends ToPredicateOps

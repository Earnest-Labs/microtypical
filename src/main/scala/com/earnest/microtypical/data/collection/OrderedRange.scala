package com.earnest.microtypical.data.collection

import com.earnest.microtypical.data._
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.syntax.scalacheck.GenGteq
import org.scalacheck.Arbitrary

case class RangeRepr [A] (start: A, end: A)

abstract sealed case class OrderedRange [A] (value: RangeRepr [A]) extends Validated

object OrderedRange extends ValidatedCompanion.Poly1Typeclass1 [OrderedRange, RangeRepr, Ordering, Errors] {
  def constraints [A] (implicit o: Ordering [A]): Constraints [RangeRepr [A]] =
    constraint [RangeRepr [A]] (r => o lteq (r.start, r.end), "start must be <= end")

  override implicit def model [A] (implicit o: Ordering [A]): Model [OrderedRange [A], RangeRepr [A], Errors] =
    Model .instance (
      validate [OrderedRange [A], RangeRepr [A]] (
        constraints (o),
        new OrderedRange [A] (_) {}),
      _.value)

  implicit def arbitrary [A] (
    implicit
    a: Arbitrary [A],
    gteq: GenGteq [A],
    o: Ordering [A]):
  Arbitrary [OrderedRange [A]] =
    Arbitrary (
      for {
        start <- a.arbitrary
        end <- gteq apply start
      } yield model applyUnsafe RangeRepr (start, end))
}

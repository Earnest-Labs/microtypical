package com.earnest.microtypical.data.collection

import cats.implicits.catsStdInstancesForSet
import com.earnest.microtypical.data._
import com.earnest.microtypical.data.validation._
import org.scalacheck.{Arbitrary, Gen}

abstract sealed case class NonEmptySet [A] (value: Set [A]) extends Validated
object NonEmptySet extends ValidatedCompanion.Poly1 [NonEmptySet, Set, Errors] {
  override implicit def model [A]: Model [NonEmptySet [A], Set [A], Errors] =
    Model .instance (
      validate [NonEmptySet [A], Set [A]] (
        ! empty [Set, A],
        new NonEmptySet [A] (_) {}),
      _.value)

  implicit def arbitrary [A] (implicit a: Arbitrary [A]): Arbitrary [NonEmptySet [A]] =
    Arbitrary (Gen .nonEmptyContainerOf [Set, A] (a.arbitrary) map applyUnsafe)
}

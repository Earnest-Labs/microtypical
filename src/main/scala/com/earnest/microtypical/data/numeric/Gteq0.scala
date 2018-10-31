package com.earnest.microtypical.data.numeric

import cats.Show
import cats.syntax.functor._
import com.earnest.microtypical.data.Bound.Inclusive
import com.earnest.microtypical.data._
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.syntax.scalacheck.GenGteq
import org.scalacheck.Arbitrary

abstract sealed case class Gteq0 [A] (value: A) extends Validated

object Gteq0 extends ValidatedCompanion.Typeclass2 [Gteq0, Numeric, Show, Errors] {
  implicit def gapFree [R] (implicit n: Numeric [R], s: Show [R]): GapFree [Gteq0 [R]] = GapFree.instance

  implicit def bounds [R] (implicit b: Bounds [R], n: Numeric [R], s: Show [R]): Bounds [Representation [Gteq0 [R], R]] =
    Bounds (Inclusive (n.zero), b.max) map Representation.apply

  def constraints [R] (implicit n: Numeric [R], s: Show [R]): Constraints [R] = gteq (n.zero) (n, s)

  override implicit def model [R] (implicit n: Numeric [R], s: Show [R]): Model [Gteq0 [R], R, Errors] =
    Model .instance (validate [Gteq0 [R], R] (constraints (n, s), new Gteq0 [R] (_) {}), _.value)

  implicit def arbitrary [N] (implicit n: Numeric [N], gteq: GenGteq [N], s: Show [N]): Arbitrary [Gteq0 [N]]  =
    Arbitrary (GenGteq to n.zero map Gteq0.applyUnsafe [N])
}

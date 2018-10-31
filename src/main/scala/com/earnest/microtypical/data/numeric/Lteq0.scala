package com.earnest.microtypical.data.numeric

import cats.Show
import cats.syntax.functor._
import com.earnest.microtypical.data.Bound.Inclusive
import com.earnest.microtypical.data._
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.syntax.scalacheck.GenLteq
import org.scalacheck.Arbitrary

abstract sealed case class Lteq0 [A] (value: A) extends Validated

object Lteq0 extends ValidatedCompanion.Typeclass2 [Lteq0, Numeric, Show, Errors] {
  implicit def gapFree [R] (implicit n: Numeric [R], s: Show [R]): GapFree [Lteq0 [R]] = GapFree.instance

  implicit def bounds [R] (implicit b: Bounds [R], n: Numeric [R], s: Show [R]): Bounds [Representation [Lteq0 [R], R]] =
    Bounds (b.min, Inclusive (n.zero)) map Representation.apply

  def constraints [R] (implicit n: Numeric [R], s: Show [R]): Constraints [R] = lteq (n.zero) (n, s)

  override implicit def model [R] (implicit n: Numeric [R], s: Show [R]): Model [Lteq0 [R], R, Errors] =
    Model .instance (validate [Lteq0 [R], R] (constraints (n, s), new Lteq0 [R] (_) {}), _.value)

  implicit def arbitrary [N] (implicit n: Numeric [N], lteq: GenLteq [N], s: Show [N]): Arbitrary [Lteq0 [N]]  =
    Arbitrary (GenLteq to n.zero map Lteq0.applyUnsafe [N])
}

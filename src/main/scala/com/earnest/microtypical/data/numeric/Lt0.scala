package com.earnest.microtypical.data.numeric

import cats.syntax.functor._
import com.earnest.microtypical.Show
import com.earnest.microtypical.data.Bound.Exclusive
import com.earnest.microtypical.data._
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.syntax.scalacheck.GenLt
import org.scalacheck.Arbitrary

abstract sealed case class Lt0 [A] (value: A) extends Validated

object Lt0 extends ValidatedCompanion.Typeclass2 [Lt0, Numeric, Show, Errors] {
  implicit def gapFree [R] (implicit n: Numeric [R], s: Show [R]): GapFree [Lt0 [R]] = GapFree.instance

  implicit def bounds [R] (implicit b: Bounds [R], n: Numeric [R], s: Show [R]): Bounds [Representation [Lt0 [R], R]] =
    Bounds (b.min, Exclusive (n.zero)) map Representation.apply

  def constraints [R] (implicit n: Numeric [R], s: Show [R]): Constraints [R] = lt (n.zero) (n, s)

  override implicit def model [R] (implicit n: Numeric [R], s: Show [R]): Model [Lt0 [R], R, Errors] =
    Model .instance (validate [Lt0 [R], R] (constraints (n, s), new Lt0 [R] (_) {}), _.value)

  def fromLteq0 [R] (implicit n: Numeric [R], s: Show [R]): Model [Lt0 [R], Lteq0 [R], Errors] =
    Model subtype (model (n, s), Lteq0 model (n, s))

  implicit def arbitrary [N] (implicit n: Numeric [N], lt: GenLt [N], s: Show [N]): Arbitrary [Lt0 [N]]  =
    Arbitrary (GenLt to n.zero map Lt0.applyUnsafe [N])
}

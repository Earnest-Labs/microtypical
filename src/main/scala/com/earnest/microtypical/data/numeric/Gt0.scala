package com.earnest.microtypical.data.numeric

import cats.Show
import cats.syntax.functor._
import com.earnest.microtypical.data.Bound.Exclusive
import com.earnest.microtypical.data._
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.syntax.scalacheck.GenGt
import org.scalacheck.Arbitrary

abstract sealed case class Gt0 [A] (value: A) extends Validated

object Gt0 extends ValidatedCompanion.Typeclass2 [Gt0, Numeric, Show, Errors] {
  implicit def gapFree [R] (implicit n: Numeric [R], s: Show [R]): GapFree [Gt0 [R]] = GapFree.instance

  implicit def bounds [R] (implicit b: Bounds [R], n: Numeric [R], s: Show [R]): Bounds [Representation [Gt0 [R], R]] =
    Bounds (Exclusive (n.zero), b.max) map Representation.apply

  def constraints [R] (implicit n: Numeric [R], s: Show [R]): Constraints [R] = gt (n.zero) (n, s)

  override implicit def model [R] (implicit n: Numeric [R], s: Show [R]): Model [Gt0 [R], R, Errors] =
    Model .instance (validate [Gt0 [R], R] (constraints (n, s), new Gt0 [R] (_) {}), _.value)

  def fromGteq0 [R] (implicit n: Numeric [R], s: Show [R]): Model [Gt0 [R], Gteq0 [R], Errors] =
    Model subtype (model (n, s), Gteq0 model (n, s))

  implicit def arbitrary [N] (implicit n: Numeric [N], gt: GenGt [N], s: Show [N]): Arbitrary [Gt0 [N]]  =
    Arbitrary (GenGt to n.zero map Gt0.applyUnsafe [N])
}

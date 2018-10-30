package com.earnest.microtypical.syntax.scalacheck

import com.earnest.microtypical.data.implicits.{microtypicalDataArbitraryFromEnumerated, microtypicalDataArbitraryLow, microtypicalDataArbitraryLow1}
import com.earnest.microtypical.data.{Bounds, Enumerated, Iso, Model, Representation}
import com.earnest.microtypical.instances.scalacheck.arbitrary.microtypicalInstancesScalacheckArbitraryMonadFilter.map
import org.scalacheck.Arbitrary
import org.scalacheck.Gen.Choose

final case class Uniform [A] (value: A)

object Uniform extends UniformLow1 {
  implicit def arbitraryFromEnumerated [A] (implicit e: Enumerated [A]): Arbitrary [Uniform [A]] =
    map (microtypicalDataArbitraryFromEnumerated (e)) (Uniform.apply)
}

trait UniformLow1 extends UniformLow2 {
  implicit def arbitraryFromBounds [A] (
    implicit
    b: Bounds [A],
    c: Choose [A],
    o: Ordering [A]):
  Arbitrary [Uniform [A]] =
    map (microtypicalDataArbitraryLow (b, c, o)) (Uniform.apply)
}

trait UniformLow2 extends UniformLow3 {
  implicit def arbitraryFromModelBounds [V, R] (
    implicit
    m: Model [V, R, _],
    b: Bounds [Representation [V, R]],
    c: Choose [Representation [V, R]],
    o: Ordering [Representation [V, R]]):
  Arbitrary [Uniform [V]] =
    map (microtypicalDataArbitraryLow1 (b, c, m, o)) (Uniform.apply)
}

trait UniformLow3 {
  implicit def arbitraryFromIso [V, R] (implicit a: Arbitrary [Uniform [R]], i: Iso [V, R]): Arbitrary [Uniform [V]] =
    map (a) (u => Uniform apply (i unapply u.value))
}

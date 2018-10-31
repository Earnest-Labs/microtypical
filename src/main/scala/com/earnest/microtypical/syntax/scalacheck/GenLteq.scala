package com.earnest.microtypical.syntax.scalacheck

import com.earnest.microtypical.data.implicits.{microtypicalDataArbitraryLow, microtypicalDataArbitraryLow1}
import com.earnest.microtypical.data.Bound.Inclusive
import com.earnest.microtypical.data.{Bounds, Model, Representation}
import org.scalacheck.Gen
import org.scalacheck.Gen.Choose

sealed trait GenLteq [A] extends (A => Gen [A])

object GenLteq {
  def apply [A] (f: A => Gen [A]) = new GenLteq [A] { override def apply (a: A) = f (a) }

  def to [A] (a: A) (implicit lteq: GenLteq [A]): Gen [A] = lteq apply a

  def constrain [A] (b: Bounds [A], a: A): Bounds [A] = Bounds (b.min, Inclusive (a))

  implicit def fromBounded [A] (implicit b: Bounds [A], c: Choose [A], o: Ordering [A]): GenLteq [A] =
    apply (value => microtypicalDataArbitraryLow (constrain (b, value), c, o) .arbitrary)

  implicit def fromBoundedRepresentaion [V, R] (
    implicit
    b: Bounds [Representation [V, R]],
    c: Choose [Representation [V, R]],
    m: Model [V, R, _],
    o: Ordering [Representation [V, R]]):
  GenLteq [V] =
    apply (value => microtypicalDataArbitraryLow1 (constrain (b, Representation [V, R] (m review value)), c, m, o) .arbitrary)
}

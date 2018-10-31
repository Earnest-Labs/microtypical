package com.earnest.microtypical.syntax.scalacheck

import com.earnest.microtypical.data.Bound.Exclusive
import com.earnest.microtypical.data.{Bounds, Model, Representation}
import com.earnest.microtypical.data.implicits.{microtypicalDataArbitraryLow, microtypicalDataArbitraryLow1}
import org.scalacheck.Gen
import org.scalacheck.Gen.Choose

sealed trait GenGt [A] extends (A => Gen [A])

object GenGt {
  def apply [A] (f: A => Gen [A]) = new GenGt [A] { override def apply (a: A) = f (a) }

  def to [A] (a: A) (implicit gt: GenGt [A]): Gen [A] = gt apply a

  def constrain [A] (b: Bounds [A], a: A): Bounds [A] = Bounds (Exclusive (a), b.max)

  implicit def fromBounded [A] (implicit b: Bounds [A], c: Choose [A], o: Ordering [A]): GenGt [A] =
    apply (value => microtypicalDataArbitraryLow (constrain (b, value), c, o) .arbitrary)

  implicit def fromBoundedRepresentaion [V, R] (
    implicit
    b: Bounds [Representation [V, R]],
    c: Choose [Representation [V, R]],
    m: Model [V, R, _],
    o: Ordering [Representation [V, R]]):
  GenGt [V] =
    apply (value => microtypicalDataArbitraryLow1 (constrain (b, Representation [V, R] (m review value)), c, m, o) .arbitrary)
}

package com.earnest.microtypical.syntax.scalacheck

import com.earnest.microtypical.data.Bound.Exclusive
import com.earnest.microtypical.data.{Bounds, Model, Representation}
import com.earnest.microtypical.data.implicits.{microtypicalDataArbitraryLow, microtypicalDataArbitraryLow1}
import org.scalacheck.Gen
import org.scalacheck.Gen.Choose

sealed trait GenLt [A] extends (A => Gen [A])

object GenLt {
  def apply [A] (f: A => Gen [A]) = new GenLt [A] { override def apply (a: A) = f (a) }

  def to [A] (a: A) (implicit lt: GenLt [A]): Gen [A] = lt apply a

  def constrain [A] (b: Bounds [A], a: A): Bounds [A] = Bounds (b.min, Exclusive (a))

  implicit def fromBounded [A] (implicit b: Bounds [A], c: Choose [A], o: Ordering [A]): GenLt [A] =
    apply (value => microtypicalDataArbitraryLow (constrain (b, value), c, o) .arbitrary)

  implicit def fromBoundedRepresentaion [V, R] (
    implicit
    b: Bounds [Representation [V, R]],
    c: Choose [Representation [V, R]],
    m: Model [V, R, _],
    o: Ordering [Representation [V, R]]):
  GenLt [V] =
    apply (value => microtypicalDataArbitraryLow1 (constrain (b, Representation [V, R] (m review value)), c, m, o) .arbitrary)
}

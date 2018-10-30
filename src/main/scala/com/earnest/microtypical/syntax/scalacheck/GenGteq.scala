package com.earnest.microtypical.syntax.scalacheck

import com.earnest.microtypical.data.Bound.Inclusive
import com.earnest.microtypical.data.{Bounds, Model, Representation}
import com.earnest.microtypical.data.implicits.{microtypicalDataArbitraryLow, microtypicalDataArbitraryLow1}
import org.scalacheck.Gen
import org.scalacheck.Gen.Choose

sealed trait GenGteq [A] extends (A => Gen [A])

object GenGteq {
  def apply [A] (f: A => Gen [A]) = new GenGteq [A] { override def apply (a: A) = f (a) }

  def to [A] (a: A) (implicit gteq: GenGteq [A]): Gen [A] = gteq apply a

  def constrain [A] (b: Bounds [A], a: A): Bounds [A] = Bounds (Inclusive (a), b.max)

  implicit def fromBoundedAndChoose [A] (implicit b: Bounds [A], c: Choose [A], o: Ordering [A]): GenGteq [A] =
    apply (value => microtypicalDataArbitraryLow (constrain (b, value), c, o) .arbitrary)

  implicit def fromBoundedRepresentaion [V, R] (
    implicit
    b: Bounds [Representation [V, R]],
    c: Choose [Representation [V, R]],
    m: Model [V, R, _],
    o: Ordering [Representation [V, R]]):
  GenGteq [V] =
    apply (value => microtypicalDataArbitraryLow1 (constrain (b, Representation [V, R] (m review value)), c, m, o) .arbitrary)
}

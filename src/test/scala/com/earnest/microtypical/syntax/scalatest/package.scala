package com.earnest.microtypical.syntax

import com.earnest.microtypical.data.Bounds
import org.scalatest.Matchers._
import org.scalatest.enablers.Containing

package object scalatest {
  implicit def boundsContains [A] (implicit o: Ordering [A]): Containing [Bounds [A]] =
    new Containing [Bounds [A]] {
      override def contains (b: Bounds [A], value: Any) = Bounds .contains (b) (value .asInstanceOf [A]) (o)
      override def containsOneOf (b: Bounds [A], values: Seq [Any]) = (values count (contains (b, _))) == 1
      override def containsNoneOf (b: Bounds [A], values: Seq [Any]) = values forall (! contains (b, _))
    }

  def checkInverse [A, B] (f: A => B, g: B => A, a: A) = g (f (a)) shouldBe a

  def checkPrismP [A, B] (p: A => Option [B], r: B => A, a: A) = p (a) map r shouldBe Some (a)
  def checkPrismR [A, B] (p: A => Option [B], r: B => A, b: B) = p (r (b)) shouldBe Some (b)

  def checkPrismPx [A, B] (p: A => Either [_, B], r: B => A, a: A) = p (a) map r shouldBe Right (a)
  def checkPrismRx [A, B] (p: A => Either [_, B], r: B => A, b: B) = p (r (b)) shouldBe Right (b)
}

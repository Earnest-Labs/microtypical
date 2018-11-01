package com.earnest.microtypical.data.collection

import com.earnest.microtypical.data.collection.OrderedRange._
import com.earnest.microtypical.data.collection.OrderedRangeTest._
import com.earnest.microtypical.syntax.scalacheck.{GenGt, GenLt, Invalid, Valid}
import com.earnest.microtypical.syntax.scalatest._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.{arbitrary => arb}
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object OrderedRangeTest {
  type V [A] = Valid [OrderedRange [A], OrderedRangeRepr [A]]
  type I [A] = Invalid [OrderedRange [A], OrderedRangeRepr [A]]
  implicit def arbitraryRangeReprValid [A] (
    implicit
    a: Arbitrary [A],
    g: GenGt [A]): Arbitrary [V [A]] =
    Arbitrary (for {
      i <- arb [A]
      j <- GenGt to i
    } yield Valid (OrderedRangeRepr (i, j)))

  implicit def arbitraryRangeReprInvalid [A] (
    implicit
    a: Arbitrary [A],
    g: GenLt [A]): Arbitrary [I [A]] =
      Arbitrary (for {
        i <- arb [A]
        j <- GenLt to i
      } yield Invalid (OrderedRangeRepr (i, j)))
}

class OrderedRangeTest extends FreeSpec with GeneratorDrivenPropertyChecks with Matchers {
  "apply" - {
    "return a Left if the set is empty and a Right if the set is not empty" in forAll { (set: Set[Int]) =>
      NonEmptySet (set) match {
        case Left (v) => set shouldBe empty
        case Right (v) =>
          set shouldNot be (empty)
          v.value shouldBe set
      }
    }
  }

  "preview (int)" - {
    "should not accept invalid" in forAll (
      (i: I [Int]) => preview (i.value) shouldBe empty)

    "andThen review should be prismatic" in forAll (
      (s: V [Int]) => checkPrismP (preview [Int], review [Int], s.value))

    "compose review should be prismatic" in forAll (
      (s: OrderedRange [Int]) => checkPrismR (preview [Int], review [Int], s))
  }

  "preview (long)" - {
    "should not accept invalid" in forAll (
      (i: I [Long]) => preview (i.value) shouldBe empty)

    "should be P prismatic" in forAll (
      (s: V [Long]) => checkPrismP (preview [Long], review [Long], s.value))

    "should be R prismatic" in forAll (
      (s: OrderedRange [Long]) => checkPrismR (preview [Long], review [Long], s))
  }
}

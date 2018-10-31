package com.earnest.microtypical.data.collection

import com.earnest.microtypical.data.collection.NonEmptySet._
import com.earnest.microtypical.data.collection.NonEmptySetTest._
import com.earnest.microtypical.syntax.scalacheck.Valid
import com.earnest.microtypical.syntax.scalatest._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.{arbitrary => arb}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

object NonEmptySetTest {
  type V = Valid [NonEmptySet [Int], Set [Int]]
  implicit val arbitrarySetValid: Arbitrary [V] = Arbitrary (arb [Set [Int]] map (_ + 0) map Valid.apply)
}

class NonEmptySetTest extends FreeSpec with GeneratorDrivenPropertyChecks with Matchers {
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

  "preview" - {
    "should not accept empty set" in (
      NonEmptySet preview Set () shouldBe None)

    "andThen review prismatic" in forAll (
      (s: V) => checkPrismP (preview [Int], review [Int], s.value))

    "compose review prismatic" in forAll (
      (s: NonEmptySet [Int]) => checkPrismR (preview [Int], review [Int], s))
  }
}

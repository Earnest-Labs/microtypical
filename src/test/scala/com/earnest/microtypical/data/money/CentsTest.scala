package com.earnest.microtypical.data.money

import com.earnest.microtypical.data.money.Cents._
import com.earnest.microtypical.data.money.CentsTest._
import com.earnest.microtypical.syntax.scalacheck.{GenGt, GenLt, Invalid, Valid}
import com.earnest.microtypical.syntax.scalatest._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

object CentsTest {
  type I = Invalid [Cents, Long]
  type V = Valid [Cents, Long]

  val oneCent = 1L

  val expectedMax: Long = maxCents
  val expectedMin: Long = minCents

  val validCentsLong: Gen [Long] = Gen choose (minCents, maxCents)
  val invalidCentsLong: Gen [Long] = Gen oneOf (GenLt to minCents, GenGt to maxCents)


  implicit val valid: Arbitrary [V] =
    Arbitrary (validCentsLong map Valid.apply)

  implicit val invalid: Arbitrary [I] =
    Arbitrary (invalidCentsLong map Invalid.apply)
}

class CentsTest  extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  "bounds" - {
    "should have the proper min" in {
      bounds should contain (expectedMin)
      bounds shouldNot contain (expectedMin - oneCent)
    }

    "should have the proper max" in {
      bounds should contain (expectedMax)
      bounds shouldNot contain (expectedMax + oneCent)
    }
  }

  "preview" - {
    "should not accept values outside the bounds" in forAll (
      (a: I) => preview (a.value) shouldBe empty)

    "andThen review should be prismatic" in forAll (
      (a: V) => checkPrismP (preview, review, a.value))

    "compose review should be prismatic" in forAll (
      (a: Cents) => checkPrismR (preview, review, a))
  }
}

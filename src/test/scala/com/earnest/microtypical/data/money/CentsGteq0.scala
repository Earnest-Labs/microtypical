package com.earnest.microtypical.data.money

import com.earnest.microtypical.data.money.CentsGteq0._
import com.earnest.microtypical.data.money.CentsGteq0Test._
import com.earnest.microtypical.syntax.scalacheck.{GenGt, GenLt, Invalid, Valid}
import com.earnest.microtypical.syntax.scalatest._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

object CentsGteq0Test {
  type I = Invalid [CentsGteq0, Long]
  type V = Valid [CentsGteq0, Long]

  val oneCent = 1L

  val expectedMax: Long = maxCents
  val expectedMin: Long = 0L

  val validCentsGteq0Long: Gen [Long] = Gen choose (0L, maxCents)
  val invalidCentsGteq0Long: Gen [Long] = Gen oneOf (GenLt to 0L, GenGt to maxCents)


  implicit val valid: Arbitrary [V] =
    Arbitrary (validCentsGteq0Long map Valid.apply)

  implicit val invalid: Arbitrary [I] =
    Arbitrary (invalidCentsGteq0Long map Invalid.apply)
}

class CentsGteq0Test  extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
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
      (a: CentsGteq0) => checkPrismR (preview, review, a))
  }
}

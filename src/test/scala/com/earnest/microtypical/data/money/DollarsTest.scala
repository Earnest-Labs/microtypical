package com.earnest.microtypical.data.money

import cats.syntax.functor._
import com.earnest.microtypical.data.money.Dollars._
import com.earnest.microtypical.data.money.DollarsTest._
import com.earnest.microtypical.syntax.scalacheck._
import com.earnest.microtypical.syntax.scalatest._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

object DollarsTest {
  type I = Invalid [Dollars, BigDecimal]
  type V = Valid [Dollars, BigDecimal]

  val expectedMax: BigDecimal = BigDecimal ("999_999_999.99" replaceAll ("_", ""))
  val expectedMin: BigDecimal = -expectedMax
  val oneCent: BigDecimal = BigDecimal ("0.01")

  val maxDollarPortion: Long = maxCents / 100
  val maxCentPortion: Long = maxCents % 100

  val validDollarPortion: Gen [BigDecimal] = genDecimalGteq0 (maxDollarPortion) map BigDecimal.apply
  val validCentsPortion: Gen [BigDecimal] = genDecimalGteq0 (maxCentPortion) map (BigDecimal (_, 2))
  val invalidDollarPortion: Gen [BigDecimal] = GenGt to maxDollarPortion map BigDecimal.apply
  val invalidCentsPortion: Gen [BigDecimal] = GenGt to maxCentPortion map (c => BigDecimal (c, c.toString.length))

  implicit val valid: Arbitrary [V] =
    Arbitrary (
      for {
        d <- validDollarPortion
        c <- validCentsPortion
        s <- Gen oneOf (-1, 1)
      } yield Valid (s * (d + c)))

  implicit val invalid: Arbitrary [I] =
    Arbitrary (
      for {
        vd <- Arbitrary.arbBool.arbitrary
        d <- if (vd) validDollarPortion else invalidDollarPortion
        c <- if (vd) invalidCentsPortion else Gen oneOf (invalidCentsPortion, validCentsPortion)
        s <- Gen oneOf (-1, 1)
      } yield Invalid (s * (d + c)))
}

class DollarsTest  extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  "bounds" - {
    val boundsR = bounds map (_.value)

    "should have the proper min" in {
      boundsR should contain (expectedMin)
      boundsR shouldNot contain (expectedMin - oneCent)
    }

    "should have the proper max" in {
      boundsR should contain (expectedMax)
      boundsR shouldNot contain (expectedMax + oneCent)
    }
  }

  "preview" - {
    "should not accept values outside the bounds" in forAll (
      (a: I) => preview (a.value) shouldBe empty)

    "andThen review should be prismatic" in forAll (
      (a: V) => checkPrismP (preview, review, a.value))

    "compose review should be prismatic" in forAll (
      (a: Dollars) => checkPrismR (preview, review, a))
  }

  "fromCents" - {
    import fromCents._

    "apply should move the decimal point 2 places" in forAll (
      (c: Cents) => apply (c) .value shouldBe BigDecimal (c.value, 2))

    "apply andThen unapply should be the identity" in forAll (
      (c: Cents) => checkInverse (apply, unapply, c))

    "apply compose unapply should be the identity" in forAll (
      (d: Dollars) => checkInverse (unapply, apply, d))
  }
}

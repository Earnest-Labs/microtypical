package com.earnest.microtypical.data.time

import java.time.{LocalDate, YearMonth}

import cats.syntax.functor._
import com.earnest.microtypical.data.time.EpochMonth._
import com.earnest.microtypical.data.time.EpochMonthTest._
import com.earnest.microtypical.instances.java.time.yearmonth._
import com.earnest.microtypical.syntax.scalacheck._
import com.earnest.microtypical.syntax.scalatest._
import org.scalacheck.Arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

object EpochMonthTest {
  type I [R] = Invalid [EpochMonth, R]
  type V [R] = Valid [EpochMonth, R]

  val expectedMinYearMonth: YearMonth = YearMonth of (1, 1)
  val expectedMaxYearMonth: YearMonth = YearMonth of (9999, 12)

  def toYearMonthTest (m: Int) = YearMonth from (LocalDate ofEpochDay 0) plusMonths m

  implicit val validInt: Arbitrary [V [Int]] = Arbitrary (genInside (bounds) map Valid.apply)
  implicit val invalidInt: Arbitrary [I [Int]] = Arbitrary (genOutside (bounds) map Invalid.apply)

  implicit val validYearMonth: Arbitrary [V [YearMonth]] = Arbitrary (genInside (ofYearMonth.bounds) map Valid.apply)
  implicit val invalidYearMonth: Arbitrary [I [YearMonth]] = Arbitrary (genOutside (ofYearMonth.bounds) map Invalid.apply)
}

class EpochMonthTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  "bounds" - {
    "should have the proper min" in {
      bounds map toYearMonthTest should contain (expectedMinYearMonth)
      bounds map toYearMonthTest shouldNot contain (expectedMinYearMonth minusMonths 1)
    }

    "should have the proper max" in {
      bounds map toYearMonthTest should contain (expectedMaxYearMonth)
      bounds map toYearMonthTest shouldNot contain (expectedMaxYearMonth plusMonths 1)
    }
  }

  "preview" - {
    "should not accept values outside the bounds" in forAll (
      (a: I [Int]) => preview (a.value) shouldBe empty)

    "andThen review should be prismatic" in forAll (
      (a: V [Int]) => checkPrismP (preview, review, a.value))

    "compose review should be prismatic" in forAll (
      (a: EpochMonth) => checkPrismR (preview, review, a))
  }

  "ofYearMonth" - {
    import ofYearMonth.{bounds, preview, review}

    "bounds" - {
      "should have the proper min" in {
        bounds should contain (expectedMinYearMonth)
        bounds shouldNot contain (expectedMinYearMonth minusMonths 1)
      }

      "should have the proper max" in {
        bounds should contain (expectedMaxYearMonth)
        bounds shouldNot contain (expectedMaxYearMonth plusMonths 1)
      }
    }

    "preview" - {
      "should not accept values outside the bounds" in forAll (
        (a: I [YearMonth]) => preview (a.value) shouldBe empty)

      "andThen review should be prismatic" in forAll (
        (a: V [YearMonth]) => checkPrismP (preview, review, a.value))

      "compose review should be prismatic" in forAll (
        (a: EpochMonth) => checkPrismR (preview, review, a))
    }
  }

  "fromYearStart" - {
    "should result in the first month of the year" in forAll (
      (a: Year) => (ofYearMonth review fromYearStart (a)) .getMonthValue shouldBe 1)

    "andThen Year.fromEpochMonth should be the identity" in forAll (
      (a: Year) => checkInverse (fromYearStart, Year.fromEpochMonth, a))

    "should be less than fromYearEnd" in forAll (
      (a: Year) => fromYearStart (a) should be < fromYearEnd (a))
  }

  "fromYearEnd" - {
    "should result in the last month of the year" in forAll (
      (a: Year) => (ofYearMonth review fromYearEnd (a)) .getMonthValue shouldBe 12)

    "andThen Year.fromEpochMonth should be the identity" in forAll (
      (a: Year) => checkInverse (fromYearEnd, Year.fromEpochMonth, a))
  }

  "length should return the number of days in the month" in forAll (
    (a: EpochMonth) => EpochMonth length a shouldBe (ofYearMonth review a) .lengthOfMonth)
}

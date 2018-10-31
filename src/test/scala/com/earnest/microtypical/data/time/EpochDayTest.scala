package com.earnest.microtypical.data.time

import java.time.LocalDate

import com.earnest.microtypical.data.time.EpochDay._
import com.earnest.microtypical.data.time.EpochDayTest._
import com.earnest.microtypical.instances.java.time.localdate._
import com.earnest.microtypical.syntax.scala.long._
import com.earnest.microtypical.syntax.scalacheck._
import com.earnest.microtypical.syntax.scalatest._
import org.scalacheck.Arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

object EpochDayTest {
  type I [R] = Invalid [EpochDay, R]
  type V [R] = Valid [EpochDay, R]

  val expectedMinLocalDate: LocalDate = LocalDate of (1, 1, 1)
  val expectedMaxLocalDate: LocalDate = LocalDate of (9999, 12, 31)

  val expectedMin: Int = expectedMinLocalDate.toEpochDay.toIntUnsafe
  val expectedMax: Int = expectedMaxLocalDate.toEpochDay.toIntUnsafe

  implicit val validInt: Arbitrary [V [Int]] = Arbitrary (genInside (bounds) map Valid.apply)
  implicit val invalidInt: Arbitrary [I [Int]] = Arbitrary (genOutside (bounds) map Invalid.apply)

  implicit val validLocalDate: Arbitrary [V [LocalDate]] = Arbitrary (genInside (ofLocalDate.bounds) map Valid.apply)
  implicit val invalidLocalDate: Arbitrary [I [LocalDate]] =Arbitrary (genOutside (ofLocalDate.bounds) map Invalid.apply)
}

class EpochDayTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  "bounds" - {
    "should have the proper min" in {
      bounds should contain (expectedMin)
      bounds shouldNot contain (expectedMin - 1)
    }

    "should have the proper max" in {
      bounds should contain (expectedMax)
      bounds shouldNot contain (expectedMax + 1)
    }
  }

  "preview" - {
    "should not accept values outside the bounds" in forAll (
      (a: I [Int]) => preview (a.value) shouldBe empty)

    "andThen review should be prismatic" in forAll (
      (a: V [Int]) => checkPrismP (preview, review, a.value))

    "compose review should be prismatic" in forAll (
      (a: EpochDay) => checkPrismR (preview, review, a))
  }

  "ofLocalDate" - {
    import ofLocalDate.{bounds, preview, review}

    "bounds" - {
      "should have the proper min" in {
        bounds should contain (expectedMinLocalDate)
        bounds shouldNot contain (expectedMinLocalDate minusDays 1)
      }

      "should have the proper max" in {
        bounds should contain (expectedMaxLocalDate)
        bounds shouldNot contain (expectedMaxLocalDate plusDays 1)
      }
    }

    "preview" - {
      "should not accept values outside the bounds" in forAll (
        (a: I [LocalDate]) => preview (a.value) shouldBe empty)

      "andThen review should be prismatic" in forAll (
        (a: V [LocalDate]) => checkPrismP (preview, review, a.value))

      "compose review should be prismatic" in forAll (
        (a: EpochDay) => checkPrismR (preview, review, a))
    }
  }

  "fromEpochMonthStart" - {
    "should result in the first day of the month" in forAll (
      (a: EpochMonth) => (ofLocalDate review fromEpochMonthStart (a)) .getDayOfMonth shouldBe 1)

    "andThen EpochMonth.fromEpochDay should be the identity" in forAll (
      (a: EpochMonth) => checkInverse (fromEpochMonthStart, EpochMonth.fromEpochDay, a))

    "should be less than fromEpochMonthEnd" in forAll (
      (a: EpochMonth) => fromEpochMonthStart (a) should be < fromEpochMonthEnd (a))
  }

  "fromEpochMonthEnd" - {
    "should result in the last day of the month" in forAll (
      (a: EpochMonth) => (ofLocalDate review fromEpochMonthEnd (a) plusDays 1) .getDayOfMonth shouldBe 1)

    "andThen EpochMonth.fromEpochDay should be the identity" in forAll (
      (a: EpochMonth) => checkInverse (fromEpochMonthEnd, EpochMonth.fromEpochDay, a))
  }

  "fromYearStart" - {
    "should result in the first day of the year" in forAll (
      (a: Year) => (ofLocalDate review fromYearStart (a)) .getDayOfYear shouldBe 1)

    "andThen Year.fromEpochDay should be the identity" in forAll (
      (a: Year) => checkInverse (fromYearStart, Year.fromEpochDay, a))

    "should be less than fromYearEnd" in forAll (
      (a: Year) => fromYearStart (a) should be < fromYearEnd (a))
  }

  "fromYearEnd" - {
    "should result in the last day of the year" in forAll (
      (a: Year) => (ofLocalDate review fromYearEnd (a)) .getDayOfYear shouldBe (Year length a))

    "andThen Year.fromEpochDay should be the identity" in forAll (
      (a: Year) => checkInverse (fromYearEnd, Year.fromEpochDay, a))
  }
}

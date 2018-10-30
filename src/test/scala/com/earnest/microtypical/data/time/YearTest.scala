package com.earnest.microtypical.data.time

import java.time.{Year => JavaYear}

import com.earnest.microtypical.data.time.Year._
import com.earnest.microtypical.data.time.YearTest._
import com.earnest.microtypical.instances.java.time.year._
import com.earnest.microtypical.syntax.scalacheck._
import com.earnest.microtypical.syntax.scalatest._
import org.scalacheck.Arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

object YearTest {
  type I [R] = Invalid [Year, R]
  type V [R] = Valid [Year, R]

  val expectedMin = 1
  val expectedMax = 9999

  implicit val validInt: Arbitrary [V [Int]] = Arbitrary (genInside (bounds) map Valid.apply)
  implicit val invalidInt: Arbitrary [I [Int]] = Arbitrary (genOutside (bounds) map Invalid.apply)

  implicit val validJavaYear: Arbitrary [V [JavaYear]] = Arbitrary (genInside (Year.ofJavaYear.bounds) map Valid.apply)
  implicit val invalidJavaYear: Arbitrary [I [JavaYear]] = Arbitrary (genOutside (Year.ofJavaYear.bounds) map Invalid.apply)
}

class YearTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
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
      (a: Year) => checkPrismR (preview, review, a))
  }

  "ofJavaYear" - {
    import ofJavaYear.{bounds, preview, review}

    "bounds" - {
      "should have the proper min" in {
        bounds should contain (JavaYear of expectedMin)
        bounds shouldNot contain (JavaYear of expectedMin - 1)
      }

      "should have the proper max" in {
        bounds should contain (JavaYear of expectedMax)
        bounds shouldNot contain (JavaYear of expectedMax + 1)
      }
    }

    "preview" - {
      "should not accept values outside the bounds" in forAll (
        (a: I [JavaYear]) => preview (a.value) shouldBe empty)

      "andThen review should be prismatic" in forAll (
        (a: V [JavaYear]) => checkPrismP (preview, review, a.value))

      "compose review should be prismatic" in forAll (
        (a: Year) => checkPrismR (preview, review, a))
    }
  }

  "length should return the number of days in the year" in forAll (
    (a: Year) => Year length a shouldBe (ofJavaYear review a) .length)
}

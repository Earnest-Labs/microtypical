package com.earnest.microtypical.data.text

import cats.instances.int.catsStdShowForInt
import com.earnest.microtypical.data.numeric.Gteq0
import com.earnest.microtypical.data.text.NonEmptyString._
import com.earnest.microtypical.data.text.NonEmptyStringTest._
import com.earnest.microtypical.syntax.scalacheck._
import com.earnest.microtypical.syntax.scalatest._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

object NonEmptyStringTest {
  type I = Invalid [NonEmptyString, String]
  type V = Valid [NonEmptyString, String]

  val withNullChar = true
  def constrainedGtEq0 = Gen choose (1, 100) map Gteq0.applyUnsafe [Int]

  implicit val validStr: Arbitrary [V] = Arbitrary (
    for {
      i <- constrainedGtEq0
      str <- genStringOfN (i, !withNullChar)
    } yield Valid (str + "a"))

  implicit val invalidStr: Arbitrary [I] =
    Arbitrary (Gen oneOf (
    Gen const "",
    genStringOfN (one (constrainedGtEq0), withNullChar) map (_ + "\u0000")) map Invalid.apply)
}

class NonEmptyStringTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  "preview" - {
    "should not accept values outside the bounds" in forAll (
      (i: I) => (NonEmptyString preview i.value) shouldBe None)

    "should be P prismatic" in forAll (
      (v: V) => checkPrismP (preview, review, v.value))

    "should be R prismatic" in forAll (
      (v: NonEmptyString) => checkPrismR (preview, review, v))
  }
}

package com.earnest.microtypical.data.numeric


import cats.instances.int.catsStdShowForInt
import com.earnest.microtypical.data.{Model, Representation}
import com.earnest.microtypical.data.numeric.Gt0._
import com.earnest.microtypical.data.numeric.Gt0IntTest._
import com.earnest.microtypical.data.validation.Errors
import com.earnest.microtypical.syntax.scalacheck._
import com.earnest.microtypical.syntax.scalatest._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object Gt0IntTest {
  type I [A] = Invalid [Gt0 [A], A]
  type V [A] = Valid [Gt0 [A], A]

  val expectedMin = 1
  val expectedMax =  Int.MaxValue

  implicit val gt0IntBounds = Gt0.bounds [Int]

  implicit val validInt: Arbitrary [V [Int]] = Arbitrary (Gen choose (expectedMin, expectedMax) map Valid.apply)
  implicit def invalidInt (implicit int: Arbitrary [Int]): Arbitrary [I [Int]] = Arbitrary (GenLt to 0 map Invalid.apply)

}

class Gt0IntTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  "bounds" - {
    "should have proper min" in {
      bounds [Int] should contain (Representation (expectedMin))
      bounds  [Int] should not contain Representation (expectedMin - 1)
    }
    "should have proper max" in {
      bounds [Int] should contain (Representation (expectedMax))
      bounds [Int] should not contain Representation (expectedMax + 1)
    }
  }

  "preview" - {
    "should not accept values outside the bounds" in forAll (
      (i: I [Int]) => (Gt0 preview [Int] i.value) shouldBe None)

    "should be P prismatic" in forAll (
      (v: V [Int]) => checkPrismP [Int, Gt0 [Int]] (preview [Int], review [Int], v.value))

    "should be R prismatic" in forAll (
      (v: Gt0 [Int]) => checkPrismR [Int, Gt0 [Int]] (preview [Int], review [Int], v))
  }

  "fromGteq0" - {
    implicit val gtEq0IntModel: Model [Gt0 [Int], Gteq0 [Int], Errors] = Gt0 .fromGteq0 [Int]
    import Gteq0.arbitrary

    "apply should succeed always" in forAll (
      (c: Gteq0 [Int]) => apply (c.value) shouldBe a [Right [_,_]])

    "andThen review should be the identity" in forAll (
      (c: Gteq0 [Int]) => checkPrismP (gtEq0IntModel.preview, gtEq0IntModel.review, c))

    "compose review should be the identity" in forAll (
      (d: Gt0 [Int]) => checkPrismR (gtEq0IntModel.preview, gtEq0IntModel.review, d))
  }
}

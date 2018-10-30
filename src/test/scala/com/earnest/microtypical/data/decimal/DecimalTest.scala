package com.earnest.microtypical.data.decimal

import com.earnest.microtypical.data.Bounds
import com.earnest.microtypical.data.decimal.Decimal._
import com.earnest.microtypical.data.decimal.DecimalTest._
import com.earnest.microtypical.instances.scalacheck.arbitrary.microtypicalInstancesScalacheckArbitraryMonadFilter.map
import com.earnest.microtypical.syntax.scalacheck._
import com.earnest.microtypical.syntax.scalatest._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import shapeless.Nat

object DecimalTest {
  type I [A] = Invalid [A, BigDecimal]
  type V [A] = Valid [A, BigDecimal]

  val scale = Nat._5 // TODO: Generate the scale

  sealed trait Units
  sealed trait Subunits

  def maxSubunits: Long = (Iterator continually 10L take scale.toInt).product - 1L

  def toValidSubunits (x: Long): BigDecimal = BigDecimal (x, scale.toInt)

  def toInvalidSubunits (x: Long): BigDecimal = BigDecimal (x, (Math abs x).toString.length)

  implicit val validUnits: Arbitrary [V [Units]] =
    map (Arbitrary.arbBigInt) (i => Valid (BigDecimal (i)))

  implicit val validSubunits: Arbitrary [V [Subunits]] =
    Arbitrary (genDecimalPlusMinus (maxSubunits) map toValidSubunits map Valid.apply)

  implicit val invalidSubunits: Arbitrary [I [Subunits]] =
    Arbitrary (genOutside (Bounds inclusive (-maxSubunits, maxSubunits)) map toInvalidSubunits map Invalid.apply)

  implicit val validBigDecimal: Arbitrary [V [Decimal [scale.N]]] =
    Arbitrary (
      Gen oneOf (
        validUnits.arbitrary map (u => Valid [Decimal [scale.N], BigDecimal] (u.value)),
        validSubunits.arbitrary map (s => Valid [Decimal [scale.N], BigDecimal] (s.value)),
        validUnits.arbitrary flatMap (u => validSubunits.arbitrary map (s => Valid [Decimal [scale.N], BigDecimal] (u.value + s.value)))))
}

class DecimalTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  "preview" - {
    "should accept values with valid units and no subunits" in forAll (
      (u: V [Units]) => preview [scale.N] (u.value) shouldBe a [Some [_]])

    "should accept values with valid units and valid subunits" in forAll (
      (u: V [Units], s: V [Subunits]) => preview [scale.N] (u.value + s.value) shouldBe a [Some [_]])

    "should accept values with no units and valid subunits" in forAll (
      (s: V [Subunits]) => preview [scale.N] (s.value) shouldBe a [Some [_]])

    "should not accept values with valid units and invalid subunits" in forAll (
      (u: V [Units], s: I [Subunits]) => preview [scale.N] (u.value + s.value) shouldBe None)

    "should not accept values with no units and invalid subunits" in forAll (
      (s: I [Subunits]) => preview [scale.N] (s.value) shouldBe None)

    "andThen review should be prismatic" in forAll (
      (d: V [Decimal [scale.N]]) => checkPrismP (preview [scale.N], review [scale.N], d.value))

    "compose review should be prismatic" in forAll (
      (d: Decimal [scale.N]) => checkPrismR (preview [scale.N], review [scale.N], d))
  }

  "choose" - {
    "should choose values between the input parameters" in forAll {
      (a: Decimal [scale.N], b: Decimal [scale.N]) =>
        val (min, max) = if (a.value <= b.value) (a, b) else (b, a)
        val x = one (choose [scale.N] choose (min, max))
        x.value should (be >= min.value and be <= max.value)
    }
  }
}

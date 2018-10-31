package com.earnest.microtypical.data.decimal

import cats.instances.int.catsStdShowForInt
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.data.{Model, Validated, ValidatedCompanion}
import org.scalacheck.Gen.Choose
import org.scalacheck.Gen.Choose.IllegalBoundsError
import org.scalacheck.{Arbitrary, Gen}
import shapeless.Succ
import shapeless.ops.nat.ToInt

import scala.math.BigDecimal.RoundingMode

abstract sealed case class Decimal [N] (value: BigDecimal) extends Validated
object Decimal extends ValidatedCompanion.Parametrized [Decimal, BigDecimal, ToInt, Succ [_], Errors] {
  def constraints [N <: Succ [_]] (implicit n: ToInt [N]): Constraints [BigDecimal] =
    context ("scale", _.scale, lteq (n.apply))

  override implicit def model [N <: Succ [_]] (implicit n: ToInt [N]): Model [Decimal [N], BigDecimal, Errors] =
    Model.instance [Decimal [N], BigDecimal, Errors] (validate (constraints (n), new Decimal [N] (_: BigDecimal) {}), _.value)

  implicit def numeric [N <: Succ [_]] (implicit n: ToInt [N]): Numeric [Decimal [N]] =
    new Numeric [Decimal [N]] with Fractional [Decimal [N]] {
      private type T = Decimal [N]

      private val num = Numeric.BigDecimalIsFractional
      private val applyUnsafe: BigDecimal => T = model [N] applyUnsafe _
      private val round: BigDecimal => T = roundBigDecimal [N]

      override def compare (x: T, y: T): Int = num compare (x.value, y.value)
      override def plus (x: T, y: T): T = applyUnsafe (num plus (x.value, y.value))
      override def minus (x: T, y: T): T = applyUnsafe (num minus (x.value, y.value))
      override def times (x: T, y: T): T = round (num times (x.value, y.value))
      override def div (x: T, y: T): T = round (num div (x.value, y.value))
      override def negate (x: T): T = applyUnsafe (num negate x.value)
      override def fromInt (x: Int): T = applyUnsafe (num fromInt x)
      override def toInt (x: T): Int = num toInt x.value
      override def toLong (x: T): Long = num toLong x.value
      override def toFloat (x: T): Float = num toFloat x.value
      override def toDouble (x: T): Double = num toDouble x.value
    }

  implicit def arbitrary [N <: Succ [_]] (implicit n: ToInt [N]): Arbitrary [Decimal [N]] =
    Arbitrary (
      for {
        i <- Arbitrary.arbBigInt.arbitrary
        s <- Gen choose (0, n.apply)
      } yield applyUnsafe (BigDecimal (i, s)))

  implicit def choose [N <: Succ [_]] (implicit n: ToInt [N]): Choose [Decimal [N]] =
    (min, max) => {
        val (l, h) = (min.value, max.value)

        if (l > h) throw new IllegalBoundsError (min, max)
        else if (l == h) Gen const min
        else (
          Gen choose (0d, 1d)
          map (_ * (h - l + BigDecimal (1, n.apply)) + l)
          map (_ setScale (n.apply, RoundingMode.FLOOR))
          map (x =>
            if (x <= l) min
            else if (x >= h) max
            else model applyUnsafe x))
    }

  def roundBigDecimal [N <: Succ [_]] (value: BigDecimal) (implicit n: ToInt [N]): Decimal [N] =
    applyUnsafe (if (value.scale <= n.apply) value else value setScale (n.apply, RoundingMode.HALF_UP)) (n)

  def roundDouble [N <: Succ [_]] (value: Double) (implicit n: ToInt [N]): Decimal [N] =
    applyUnsafe (BigDecimal (value) setScale (n.apply, RoundingMode.HALF_UP)) (n)
}

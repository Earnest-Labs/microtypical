package com.earnest.microtypical.data.text

import com.earnest.microtypical.data._
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.syntax.scalacheck.Uniform
import org.scalacheck.{Arbitrary, Gen}

abstract sealed case class CodePoint (value: Int) extends Validated

object CodePoint extends ValidatedCompanion [CodePoint, Int, Errors] {
  val NULL = new CodePoint (0) {}

  val lowBasicRange = (0, 0xd7ff)
  val surrogateRange = (0xd800, 0xdfff)
  val highBasicRange = (0xe000, 0xffff)
  val supplementaryRange = (0x010000, 0x10ffff)

  val lowBasicAsciiRange = (lowBasicRange._1, lowBasicRange._1 + 127)
  val lowBasicAsciiRangeNoNull = (lowBasicAsciiRange._1 + 1, lowBasicAsciiRange._2)
  val lowBasicNonAsciiRange = (lowBasicAsciiRange._2 + 1, lowBasicRange._2)

  private val pairSize = 1 << 10
  private val pairHighZero = surrogateRange._1
  private val pairLowZero = pairHighZero + pairSize

  implicit val bounds: Bounds [CodePoint] =
    Bounds inclusive (
      new CodePoint (lowBasicRange._1) {},
      new CodePoint (supplementaryRange._2) {})

  val constraints: Constraints [Int] =
    constraint [Int] (isBasic, "be in the basic range") or
    constraint [Int] (isSupplementary, "be in the supplementary range")

  override implicit val model: Model [CodePoint, Int, Errors] =
    Model .instance (validate [CodePoint, Int] (constraints, new CodePoint (_) {}), _.value)

  def rangeToSize (range: (Int, Int)): Int = range._2 - range._1 + 1

  def rangeToGen (range: (Int, Int)): Gen [CodePoint] = Gen choose (range._1, range._2) map applyUnsafe

  def inRange (value: Int, range: (Int, Int)) : Boolean = range._1 <= value && value <= range._2

  def isBasic (value: Int): Boolean = inRange (value, lowBasicRange) || inRange (value, highBasicRange)

  def isSupplementary (value: Int): Boolean = inRange (value, supplementaryRange)

  def show (codePoint: CodePoint): String = codePoint.value match {
    case b if isBasic (b) => b.toChar.toString
    case s if isSupplementary (s) =>
      val o = s - supplementaryRange._1
      val h = (o / pairSize + pairHighZero) .toChar
      val l = (o % pairSize + pairLowZero) .toChar
      s"${h}${l}"
  }

  implicit def arbitrary: Arbitrary [CodePoint] =
    Arbitrary (
      Gen frequency (
        5 -> lowBasicAsciiRangeNoNull,
        1 -> lowBasicNonAsciiRange,
        1 -> highBasicRange,
        1 -> supplementaryRange)
      flatMap rangeToGen)

  implicit def uniform: Arbitrary [Uniform [CodePoint]] =
    Arbitrary (
      Gen frequency (
        List (
          lowBasicRange,
          highBasicRange,
          supplementaryRange)
        map (r => (
          rangeToSize (r),
          rangeToGen (r) map Uniform.apply)) : _*))
}

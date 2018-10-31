package com.earnest.microtypical.data.time

import java.time.temporal.ChronoUnit.MICROS
import java.time.{Duration, Instant}

import com.earnest.microtypical.data.microtypicalDataArbitraryLow
import com.earnest.microtypical.data.time.EpochMicrosecond._
import com.earnest.microtypical.data.time.EpochMicrosecondTest._
import com.earnest.microtypical.instances.java.time.instant._
import com.earnest.microtypical.syntax.scalacheck._
import com.earnest.microtypical.syntax.scalatest._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

object EpochMicrosecondTest {
  type I [R] = Invalid [EpochMicrosecond, R]
  type V [R] = Valid [EpochMicrosecond, R]

  def microToNano = (Duration of (1, MICROS)) .toNanos

  def usToInstant (us: Long): Instant = Instant.EPOCH plus (us, MICROS)

  val expectedMin: Long = 0L
  val expectedMax: Long = Long.MaxValue

  val expectedMinInstant: Instant = usToInstant (expectedMin)
  val expectedMaxInstant: Instant = usToInstant (expectedMax)

  implicit val validLong: Arbitrary [V [Long]] = Arbitrary (genInside (bounds) map Valid.apply)
  implicit val invalidLong: Arbitrary [I [Long]] = Arbitrary (genOutside (bounds) map Invalid.apply)

  implicit val validInstant: Arbitrary [V [Instant]] =
    Arbitrary (genInside (ofInstant.bounds) map ofInstant.truncateToMicros map Valid.apply)
  implicit val invalidInstant: Arbitrary [I [Instant]] =
    Arbitrary (genOutside (ofInstant.bounds) map Invalid.apply)

  case class BadNanos (value: Long)
  object BadNanos {
    implicit val arbitrary: Arbitrary [BadNanos] = Arbitrary (Gen choose (1L, (Duration of (1, MICROS)) .toNanos - 1) map apply)
  }
}

class EpochMicrosecondTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  "bounds" - {
    "should have the proper min" in {
      bounds should contain (expectedMin)
      bounds shouldNot contain (expectedMin - 1)
    }

    "should have the proper max" in (
      bounds should contain (expectedMax))
  }

  "preview" - {
    "should not accept values outside the bounds" in forAll (
      (a: I [Long]) => preview (a.value) shouldBe empty)

    "andThen review should be prismatic" in forAll (validLong.arbitrary)(
      (a: V [Long]) => checkPrismP (preview, review, a.value))

    "compose review should be prismatic" in forAll (
      (a: EpochMicrosecond) => checkPrismR (preview, review, a))
  }

  "ofInstant" - {
    import ofInstant.{bounds, preview, review, truncateToMicros}

    "bounds" - {
      "should have the proper min" in {
        bounds should contain (expectedMinInstant)
        bounds shouldNot contain (expectedMinInstant minusNanos 1)
      }

      "should have the proper max" in {
        bounds should contain (expectedMaxInstant)
        bounds shouldNot contain (expectedMaxInstant plusNanos 1)
      }
    }

    "preview" - {
      "should not accept values outside the bounds" in forAll (
        (a: I [Instant]) => preview (a.value) shouldBe empty)

      "should not accept values outside the bounds with zero nanos" in forAll (
        (a: I [Instant]) =>
          whenever (a.value.getEpochSecond != expectedMaxInstant.getEpochSecond)
          (preview (truncateToMicros (a.value)) shouldBe empty))

      "should not accept values inside the bounds with non-zero nanos" in forAll (
        (a: V [Instant], ns: BadNanos) => preview (a.value plusNanos ns.value) shouldBe empty)

      "andThen review should be prismatic for values with zero nanos" in forAll (
        (a: V [Instant]) => checkPrismP (preview, review, a.value))

      "compose review should be prismatic" in forAll (
        (a: EpochMicrosecond) => checkPrismR (preview, review, a))
    }

    "truncateToMicros" - {
      "should not affect epoch second" in forAll (
        (a: Instant) => truncateToMicros (a) .getEpochSecond shouldBe a.getEpochSecond)

      "should result in nanos divisible by micros" in forAll (
        (a: Instant) => truncateToMicros (a) .getNano % microToNano shouldBe 0)

      "should result in nanos equal to the input nanos with no sub-micro portion" in forAll (
        (a: Instant) => truncateToMicros (a) .getNano shouldBe a.getNano - a.getNano % microToNano)
    }
  }
}

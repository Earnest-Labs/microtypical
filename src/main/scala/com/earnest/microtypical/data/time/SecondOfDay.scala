package com.earnest.microtypical.data.time

import java.time.LocalTime
import java.time.format.DateTimeFormatter

import com.earnest.microtypical.Show
import com.earnest.microtypical.data.BooleanExpression.Pure
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.data.{Model, Validated}
import org.scalacheck.{Arbitrary, Gen}

import scala.util.{Failure, Success, Try}

abstract sealed case class SecondOfDay (value: Int) extends Validated

object SecondOfDay extends BoundedCompanion.Inclusive [SecondOfDay, Int] {
  override def min = 0
  override def max = 60 * 60 * 24 - 1

  override def ordering: Ordering [Int] = implicitly
  override def show: Show [Int] = implicitly

  override protected [this] def wrap (i: Int): SecondOfDay = new SecondOfDay (i) {}
  override protected [this] def unwrap (v: SecondOfDay): Int = v.value

  implicit def arbitrary: Arbitrary [SecondOfDay] = Arbitrary (Gen choose (min, max) map applyUnsafe)

  val ofLocalTime: Model [SecondOfDay, LocalTime, Errors] =
    Model .instance (
      validate [SecondOfDay, LocalTime] (
        ! constraint [LocalTime] (_.getNano != 0, "have partial seconds"),
        v => wrap (v.toSecondOfDay)),
      LocalTime ofSecondOfDay _.value)

  val ofIso8601: Model [SecondOfDay, String, Errors] =
    Model .instance (
      r =>
        Try (LocalTime parse (r, DateTimeFormatter.ISO_LOCAL_TIME)) match {
          case Success (t) => ofLocalTime (t)
          case Failure (_) => Left (Pure ("be a well formed local time"))
        },
      v => ofLocalTime review v format DateTimeFormatter.ISO_LOCAL_TIME)
}

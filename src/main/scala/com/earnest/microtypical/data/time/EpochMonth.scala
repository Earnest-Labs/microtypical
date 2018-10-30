package com.earnest.microtypical.data.time

import java.time.temporal.ChronoUnit
import java.time.{Month, YearMonth}

import cats.Show
import cats.instances.int.catsStdShowForInt
import com.earnest.microtypical.data._
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.instances.java.time.yearmonth._
import com.earnest.microtypical.syntax.scala.long._

abstract sealed case class EpochMonth (value: Int) extends Validated

object EpochMonth extends BoundedCompanion.Inclusive [EpochMonth, Int] {
  override protected [this] def wrap (r: Int): EpochMonth = new EpochMonth (r) {}
  override protected [this] def unwrap (v: EpochMonth): Int = v.value
  override def ordering: Ordering [Int] = implicitly
  override def show: Show [Int] = implicitly

  def minYearMonth: YearMonth = YearMonth of (minYear, Month.JANUARY)
  def maxYearMonth: YearMonth = YearMonth of (maxYear, Month.DECEMBER)

  override def min: Int = fromYearMonthUnsafe (minYearMonth)
  override def max: Int = fromYearMonthUnsafe (maxYearMonth)

  private def yearMonthZero: YearMonth = YearMonth of (epochYear, Month.JANUARY)
  private def fromYearMonthUnsafe (m: YearMonth): Int = (yearMonthZero until (m, ChronoUnit.MONTHS)).toIntUnsafe
  private def toYearMonth (m: Int): YearMonth = yearMonthZero plusMonths m

  def fromEpochDay (d: EpochDay): EpochMonth = ofYearMonth applyUnsafe (YearMonth from (EpochDay.ofLocalDate review d))

  def fromYearStart (y: Year): EpochMonth = ofYearMonth applyUnsafe (YearMonth of (y.value, Month.JANUARY))
  def fromYearEnd (y: Year): EpochMonth = ofYearMonth applyUnsafe (YearMonth of (y.value, Month.DECEMBER))

  def length (m: EpochMonth): Int = (ofYearMonth review m) .lengthOfMonth

  object ofYearMonth extends BoundedCompanion.Inclusive [EpochMonth, YearMonth] {
    override protected [this] def wrap (r: YearMonth): EpochMonth = EpochMonth applyUnsafe fromYearMonthUnsafe (r)
    override protected [this] def unwrap (v: EpochMonth): YearMonth = toYearMonth (v.value)
    override def ordering: Ordering [YearMonth] = implicitly
    override def show: Show [YearMonth] = implicitly

    override def min: YearMonth = minYearMonth
    override def max: YearMonth = maxYearMonth
  }
}

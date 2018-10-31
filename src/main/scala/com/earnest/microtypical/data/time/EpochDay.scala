package com.earnest.microtypical.data.time

import java.time.LocalDate

import cats.Show
import cats.instances.int.catsStdShowForInt
import com.earnest.microtypical.data._
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.instances.java.time.localdate._
import com.earnest.microtypical.syntax.scala.long._

abstract sealed case class EpochDay (value: Int) extends Validated

object EpochDay extends BoundedCompanion.Inclusive [EpochDay, Int] {
  override protected [this] def wrap (r: Int): EpochDay = new EpochDay (r) {}
  override protected [this] def unwrap (v: EpochDay): Int = v.value
  override def ordering: Ordering [Int] = implicitly
  override def show: Show [Int] = implicitly

  def minLocalDate: LocalDate = LocalDate of (minYear, 1, 1)
  def maxLocalDate: LocalDate = LocalDate of (maxYear, 12, 31)

  override def min: Int = fromLocalDateUnsafe (minLocalDate)
  override def max: Int = fromLocalDateUnsafe (maxLocalDate)

  private def fromLocalDateUnsafe (d: LocalDate): Int = d.toEpochDay.toIntUnsafe

  def fromEpochMonthStart (m: EpochMonth): EpochDay = ofLocalDate applyUnsafe (EpochMonth.ofYearMonth review m atDay 1)
  def fromEpochMonthEnd (m: EpochMonth): EpochDay = ofLocalDate applyUnsafe (EpochMonth.ofYearMonth review m) .atEndOfMonth

  def fromYearStart (y: Year): EpochDay = ofLocalDate applyUnsafe (LocalDate ofYearDay (y.value, 1))
  def fromYearEnd (y: Year): EpochDay = ofLocalDate applyUnsafe (LocalDate ofYearDay (y.value, Year length y))

  object ofLocalDate extends BoundedCompanion.Inclusive [EpochDay, LocalDate] {
    override protected [this] def wrap (r: LocalDate): EpochDay = EpochDay applyUnsafe fromLocalDateUnsafe (r)
    override protected [this] def unwrap (v: EpochDay): LocalDate = LocalDate ofEpochDay v.value
    override def ordering: Ordering [LocalDate] = implicitly
    override def show: Show [LocalDate] = implicitly

    override def min: LocalDate = minLocalDate
    override def max: LocalDate = maxLocalDate
  }
}

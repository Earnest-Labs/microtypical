package com.earnest.microtypical.data.time

import java.time.{Year => JavaYear}

import cats.Show
import cats.instances.int.catsStdShowForInt
import com.earnest.microtypical.data._
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.instances.java.time.year._

abstract sealed case class Year (value: Int) extends Validated

object Year extends BoundedCompanion.Inclusive [Year, Int] {
  override protected [this] def wrap (r: Int): Year = new Year (r) {}
  override protected [this] def unwrap (v: Year): Int = v.value
  override def ordering: Ordering [Int] = implicitly
  override def show: Show [Int] = implicitly

  override def min: Int = minYear
  override def max: Int = maxYear

  def fromEpochDay (d: EpochDay): Year = model applyUnsafe (EpochDay.ofLocalDate review d) .getYear
  def fromEpochMonth (m: EpochMonth): Year = model applyUnsafe (EpochMonth.ofYearMonth review m) .getYear

  def length (y: Year): Int = (ofJavaYear review y) .length

  object ofJavaYear extends BoundedCompanion.Inclusive [Year, JavaYear] {
    override protected [this] def wrap (r: JavaYear): Year = Year applyUnsafe r.getValue
    override protected [this] def unwrap (v: Year): JavaYear = JavaYear of v.value
    override def ordering: Ordering [JavaYear] = implicitly
    override def show: Show [JavaYear] = implicitly

    override def min: JavaYear = JavaYear of Year.min
    override def max: JavaYear = JavaYear of Year.max
  }
}

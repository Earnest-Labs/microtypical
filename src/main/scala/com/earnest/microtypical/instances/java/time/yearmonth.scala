package com.earnest.microtypical.instances.java.time

import java.time.temporal.ChronoUnit.MONTHS
import java.time.{LocalDate, Year, YearMonth}

import cats.Show
import com.earnest.microtypical.data.Bounds
import org.scalacheck.Gen.Choose

trait yearmonth {
  private val yearMonthZero = YearMonth from (LocalDate ofEpochDay 0)

  implicit val microtypicalInstancesJavaTimeYearMonthBounds: Bounds [YearMonth] =
    Bounds inclusive (YearMonth of (Year.MIN_VALUE, 1), YearMonth of (Year.MAX_VALUE, 12))

  implicit val microtypicalInstancesJavaTimeYearMonthChoose: Choose [YearMonth] =
    Choose .xmap [Long, YearMonth] (yearMonthZero.plusMonths, yearMonthZero until (_, MONTHS))

  implicit val microtypicalInstancesJavaTimeYearMonthShow: Show [YearMonth] = Show.fromToString
}

object yearmonth extends yearmonth

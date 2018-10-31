package com.earnest.microtypical.instances.java.time

import java.time.temporal.ChronoUnit.MONTHS
import java.time.{LocalDate, YearMonth}

import org.scalacheck.Gen.Choose

trait yearmonth {
  private val yearMonthZero = YearMonth from (LocalDate ofEpochDay 0)

  implicit val microtypicalInstancesJavaTimeYearMonthChoose: Choose [YearMonth] =
    Choose .xmap [Long, YearMonth] (yearMonthZero.plusMonths, yearMonthZero until (_, MONTHS))
}

object yearmonth extends yearmonth

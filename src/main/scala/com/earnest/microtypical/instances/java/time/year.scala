package com.earnest.microtypical.instances.java.time

import java.time.Year

import org.scalacheck.Gen.Choose

trait year {
  implicit val microtypicalInstancesJavaTimeYearChoose: Choose [Year] =
    Choose .xmap [Int, Year] (Year.of, _.getValue)
}

object year extends year

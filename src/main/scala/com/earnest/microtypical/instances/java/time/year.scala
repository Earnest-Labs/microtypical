package com.earnest.microtypical.instances.java.time

import java.time.Year

import cats.Show
import cats.syntax.functor._
import com.earnest.microtypical.data.Bounds
import org.scalacheck.Gen.Choose

trait year {
  implicit val microtypicalInstancesJavaTimeYearBounds: Bounds [Year] =
    Bounds inclusive (Year.MIN_VALUE, Year.MAX_VALUE) map Year.of

  implicit val microtypicalInstancesJavaTimeYearChoose: Choose [Year] =
    Choose .xmap [Int, Year] (Year.of, _.getValue)

  implicit val microtypicalInstancesJavaTimeYearShow: Show [Year] = Show.fromToString
}

object year extends year

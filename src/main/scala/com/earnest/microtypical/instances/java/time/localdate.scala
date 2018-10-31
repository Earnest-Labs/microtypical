package com.earnest.microtypical.instances.java.time

import java.time.LocalDate

import cats.Show
import com.earnest.microtypical.data.Bounds
import org.scalacheck.Gen.Choose

trait localdate {
  implicit val microtypicalInstancesJavaTimeLocalDateBounds: Bounds [LocalDate] =
    Bounds inclusive (LocalDate.MIN, LocalDate.MAX)

  implicit val microtypicalInstancesJavaTimeLocalDateChoose: Choose [LocalDate] =
    Choose .xmap [Long, LocalDate] (LocalDate.ofEpochDay, _.toEpochDay)

  implicit val microtypicalInstancesJavaTimeLocalDateOrdering: Ordering [LocalDate] =
    Ordering comparatorToOrdering ((l, r) => l compareTo r)

  implicit val microtypicalInstancesJavaTimeLocalDateShow: Show [LocalDate] = Show.fromToString
}

object localdate extends localdate

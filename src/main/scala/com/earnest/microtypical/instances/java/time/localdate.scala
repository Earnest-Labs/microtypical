package com.earnest.microtypical.instances.java.time

import java.time.LocalDate

import org.scalacheck.Gen.Choose

trait localdate {
  implicit val microtypicalInstancesJavaTimeLocalDateChoose: Choose [LocalDate] =
    Choose .xmap [Long, LocalDate] (LocalDate.ofEpochDay, _.toEpochDay)

  implicit val microtypicalInstancesJavaTimeLocalDateOrdering: Ordering [LocalDate] =
    Ordering comparatorToOrdering ((l, r) => l compareTo r)
}

object localdate extends localdate

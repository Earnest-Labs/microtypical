package com.earnest.microtypical.instances.java.time

import java.time.Instant

import cats.Show
import com.earnest.microtypical.data.Bounds
import org.scalacheck.Gen
import org.scalacheck.Gen.Choose


trait instant {
  implicit val microtypicalInstancesJavaTimeInstantBounds: Bounds [Instant] =
    Bounds inclusive (Instant.MIN, Instant.MAX)

  implicit val microtypicalInstancesJavaTimeInstantChoose: Choose [Instant] =
    (min: Instant, max: Instant) =>
      for {
        s <- Gen choose (min.getEpochSecond, max.getEpochSecond)
        ns <- Gen choose (
          if (s == min.getEpochSecond) min.getNano else 0,
          if (s == max.getEpochSecond) max.getNano else 999999999)
      } yield Instant ofEpochSecond (s, ns)

  implicit val microtypicalInstancesJavaTimeInstantShow: Show [Instant] = Show.fromToString
}

object instant extends instant

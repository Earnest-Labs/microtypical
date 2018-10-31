package com.earnest.microtypical.syntax.scalacheck

import org.scalacheck.Arbitrary

case class DistinctPair [A] (_1: A, _2: A)

object DistinctPair extends DistinctPairLow {
  implicit def arbitrary [A] (implicit a: Arbitrary [Uniform [A]]): Arbitrary [DistinctPair [A]] =
    Arbitrary (arbitraryLow (a) .arbitrary map (d => DistinctPair (d._1.value, d._2.value)))
}

trait DistinctPairLow {
  implicit def arbitraryLow [A] (implicit a: Arbitrary [A]): Arbitrary [DistinctPair [A]] =
    Arbitrary (
      for {
        _1 <- a.arbitrary
        _2 <- a.arbitrary
        if _1 != _2
      } yield DistinctPair (_1, _2))
}

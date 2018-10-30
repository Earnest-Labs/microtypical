package com.earnest.microtypical.syntax.scalacheck

import com.earnest.microtypical.data.Iso
import org.scalacheck.Arbitrary

case class Independent [A] (value: A)

object Independent {
  implicit def arbitraryFromIso [A, B] (
    implicit
    b: Arbitrary [Independent [B]],
    i: Iso [A, B],
  ): Arbitrary [Independent [A]] =
    Arbitrary (b.arbitrary map (i unapply _.value) map apply)
}

package com.earnest.microtypical.data

sealed trait GapFree [A]

object GapFree {
  def apply [A] (implicit gf: GapFree [A]) = gf

  def instance [A]: GapFree [A] = new GapFree [A] {}
}

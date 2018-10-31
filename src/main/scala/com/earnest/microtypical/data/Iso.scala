package com.earnest.microtypical.data

trait Iso [A, B] {
  val apply: A => B
  val unapply: B => A
}

object Iso extends {
  def apply [A, B] (implicit i: Iso [A, B]) = i

  def instance [A, B] (ab: A => B, ba: B => A): Iso [A, B] =
    new Iso [A, B] {
      override val apply = ab
      override val unapply = ba
    }

  def reflexive [A]: Iso [A, A] = Iso .instance (identity, identity)

  def symmetric [A, B] (ab: Iso [A, B]): Iso [B, A] =
    Iso instance (ab.unapply, ab.apply)

  def transitive [A, B, C] (ab: Iso [A, B], bc: Iso [B, C]): Iso [A, C] =
    Iso instance (ab.apply andThen bc.apply, bc.unapply andThen ab.unapply)
}

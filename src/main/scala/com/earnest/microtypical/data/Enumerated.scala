package com.earnest.microtypical.data

sealed trait Enumerated [A] {
  val values: Set [A]
}

object Enumerated {
  def apply [A] (implicit e: Enumerated [A]) = e

  def instance [A] (_values: Set [A]): Enumerated [A] = new Enumerated [A] { override val values = _values }

  implicit val boolean: Enumerated [Boolean] = instance (Set (true, false))
  implicit val unit: Enumerated [Unit] = instance (Set (()))
}

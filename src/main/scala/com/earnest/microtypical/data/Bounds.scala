package com.earnest.microtypical.data

import cats.Functor
import cats.syntax.functor._
import com.earnest.microtypical.data.Bound.{Exclusive, Inclusive}

case class Bounds [A] (min: Bound [A], max: Bound [A])

object Bounds {
  implicit val functor: Functor [Bounds] = new Functor [Bounds] {
    override def map [A, B] (fa: Bounds [A]) (f: A => B) = apply (fa.min map f, fa.max map f)
  }

  def exclusive [A] (minValue: A, maxValue: A): Bounds [A] = apply (Exclusive (minValue), Exclusive (maxValue))

  def inclusive [A] (minValue: A, maxValue: A): Bounds [A] = apply (Inclusive (minValue), Inclusive (maxValue))

  def contains [A] (bounds: Bounds [A]) (value: A) (implicit o: Ordering [A]): Boolean =
    (bounds.min match {
      case Exclusive (e) => o gt (value, e)
      case Inclusive (i) => o gteq (value, i)
    }) &&
    (bounds.max match {
      case Exclusive (e) => o lt (value, e)
      case Inclusive (i) => o lteq (value, i)
    })

  def canProveEmpty [A] (bounds: Bounds [A]) (implicit o: Ordering [A]): Boolean =
    (o gteq (bounds.min.value, bounds.max.value)) &&
      ! contains (bounds) (bounds.min.value) &&
      ! contains (bounds) (bounds.max.value)

  implicit val char: Bounds [Char] = inclusive (Char.MinValue, Char.MaxValue)

  implicit val byte: Bounds [Byte] = inclusive (Byte.MinValue, Byte.MaxValue)
  implicit val short: Bounds [Short] = inclusive (Short.MinValue, Short.MaxValue)
  implicit val int: Bounds [Int] = inclusive (Int.MinValue, Int.MaxValue)
  implicit val long: Bounds [Long] = inclusive (Long.MinValue, Long.MaxValue)

  implicit val float: Bounds [Float] = inclusive (Float.MinValue, Float.MaxValue)
  implicit val double: Bounds [Double] = inclusive (Double.MinValue, Double.MaxValue)
}

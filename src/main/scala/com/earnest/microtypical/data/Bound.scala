package com.earnest.microtypical.data

import cats.Functor

sealed trait Bound [A] { val value: A }

object Bound {
  case class Exclusive [A] (value: A) extends Bound [A]
  case class Inclusive [A] (value: A) extends Bound [A]

  implicit val functor: Functor [Bound] = new Functor [Bound] {
    override def map [A, B] (fa: Bound [A]) (f: A => B) =
      fa match {
        case Exclusive (value) => Exclusive (f (value))
        case Inclusive (value) => Inclusive (f (value))
      }
  }

  def flip [A] (bound: Bound [A]): Bound [A] =
    bound match {
      case Exclusive (value) => Inclusive (value)
      case Inclusive (value) => Exclusive (value)
    }
}

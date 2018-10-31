package com.earnest.microtypical.data

import cats.Functor

case class Representation [V, R] (value: R)

object Representation {
  implicit def functor [V]: Functor [Representation [V, ?]] = new Functor [Representation [V, ?]] {
    override def map [R, RR] (fa: Representation [V, R]) (f: R => RR): Representation [V, RR] = apply (f (fa.value))
  }

  implicit def ordering [V, R] (implicit o: Ordering [R]): Ordering [Representation [V, R]] =
    Ordering .by [Representation [V, R], R] (_.value) (o)
}

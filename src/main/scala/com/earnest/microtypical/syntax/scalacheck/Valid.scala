package com.earnest.microtypical.syntax.scalacheck

import cats.Monad

import scala.annotation.tailrec

final case class Valid [V, R] (value: R)

object Valid {
  implicit def monadInstance [V]: Monad [Valid [V, ?]] = new Monad [Valid [V, ?]] {
    def pure [A] (a: A): Valid [V, A] = apply (a)

    def flatMap [A, B] (fa: Valid [V, A]) (f: A => Valid [V, B]): Valid [V, B] = f (fa.value)

    @tailrec override def tailRecM [A, B] (a: A) (f: A => Valid [V, Either [A, B]]): Valid [V, B] =
      f (a) .value match {
        case Left (next) => tailRecM (next) (f)
        case Right (done) => apply (done)
      }
  }
}

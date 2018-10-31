package com.earnest.microtypical.syntax.scalacheck

import cats.Monad

import scala.annotation.tailrec

case class Invalid [V, R] (value: R)

object Invalid {
  implicit def monadInstance [V]: Monad [Invalid [V, ?]] = new Monad [Invalid [V, ?]] {
    def pure [A] (a: A): Invalid [V, A] = apply (a)

    def flatMap [A, B] (fa: Invalid [V, A]) (f: A => Invalid [V, B]): Invalid [V, B] = f (fa.value)

    @tailrec override def tailRecM [A, B] (a: A) (f: A => Invalid [V, Either [A, B]]): Invalid [V, B] =
      f (a) .value match {
        case Left (next) => tailRecM (next) (f)
        case Right (done) => apply (done)
      }
  }
}


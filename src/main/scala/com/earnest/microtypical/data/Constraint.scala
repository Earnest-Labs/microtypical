package com.earnest.microtypical.data

import cats.arrow.Profunctor
import com.earnest.microtypical.Show.microtypicalContravariantForShow.contramap
import com.earnest.microtypical.data.BooleanExpression.Pure
import com.earnest.microtypical.{Predicate, Show}

import scala.language.implicitConversions

case class Constraint [A, E] (predicate: Predicate [A], error: E)

object Constraint {
  implicit val profunctor: Profunctor [Constraint] = new Profunctor [Constraint] {
    override def dimap [A, B, C, D] (c: Constraint [A, B]) (f: C => A) (g: B => D): Constraint [C, D] =
      Constraint (c.predicate compose f, g apply c.error)
  }

  implicit def show [A, E] (implicit showE: Show [E]): Show [Constraint [A, E]] = contramap (showE) (_.error)

  implicit def toBooleanExpr [A, E] (c: Constraint [A, E]): BooleanExpression [Constraint [A, E]] = Pure (c)

  implicit class Ops [A, E] (val self: Constraint [A, E]) extends AnyVal {
    type Expr = BooleanExpression [Constraint [A, E]]

    def unary_! : Expr = ! Pure (self)

    def and (other: Expr): Expr = Pure (self) and other
    def or (other: Expr): Expr = Pure (self) or other
    def xor (other: Expr): Expr = Pure (self) xor other
    def implies (other: Expr): Expr = Pure (self) implies other
    def impliedBy (other: Expr): Expr = Pure (self) impliedBy other
    def iff (other: Expr): Expr = Pure (self) iff other

    def && (other: Expr): Expr = and (other)
    def || (other: Expr): Expr = or (other)
    def ^ (other: Expr): Expr = xor (other)
    def --> (other: Expr): Expr = implies (other)
    def <-- (other: Expr): Expr = impliedBy (other)
    def <--> (other: Expr): Expr = iff (other)
  }
}

package com.earnest.microtypical.data

import cats.instances.option.catsStdInstancesForOption
import cats.{Apply, Monad, Semigroup, SemigroupK}
import com.earnest.microtypical.{Predicate, Show}
import com.earnest.microtypical.data.BooleanExpression.internal._
import org.scalacheck.{Arbitrary, Gen}

sealed trait BooleanExpression [A]
object BooleanExpression {
  case class And [A] (left: Expr [A], right: Expr [A]) extends Expr [A]
  case class Context [A] (context: String, expr: Expr [A]) extends Expr [A]
  case class Not [A] (expr: Expr [A]) extends Expr [A]
  case class Or [A] (left: Expr [A], right: Expr [A]) extends Expr [A]
  case class Pure [A] (value: A) extends Expr [A]

  implicit val monad: Monad [Expr] = new Monad [Expr] {
    override def pure [A] (a: A): Expr [A] = Pure (a)
    override def flatMap [A, B] (expr: Expr [A]) (f: (A) => Expr [B]): Expr [B] =
      cata [A, Expr [B]] (expr, And.apply, Context.apply, Not.apply, Or.apply, f)
    override def tailRecM [A, B] (a: A) (f: (A) => Expr [Either [A, B]]): Expr [B] =
      flatMap (f (a)) {
        case Right (b) => pure (b)
        case Left (nextA) => tailRecM (nextA) (f)
      }
  }

  implicit val semigroupAnd: SemigroupK [Expr] = new SemigroupK [Expr] {
    override def combineK [A] (a: Expr [A], b: Expr [A]): Expr [A] = And (a, b)
  }

  val semigroupOr: SemigroupK [Expr] = new SemigroupK [Expr] {
    override def combineK [A] (a: Expr [A], b: Expr [A]): Expr [A] = Or (a, b)
  }

  implicit def show [A] (implicit showA: Show [A]): Show [Expr [A]] =
    Show show (prettyExpr (_, None, new StringBuilder, 0) (showA) result ())

  implicit class Ops [A] (val self: BooleanExpression [A]) extends AnyVal {
    def run (p: Predicate [A]): Boolean = BooleanExpression run (self, p)

    def and (other: Expr [A]): Expr [A] = And (self, other)
    def or (other: Expr [A]): Expr [A] = Or (self, other)
    def xor (other: Expr [A]): Expr [A] = Or (And (self, Not (other)), And (Not (self), other))
    def implies (other: Expr [A]): Expr [A] = Or (Not (self), other)
    def impliedBy (other: Expr [A]): Expr [A] = Or (self, Not (other))
    def iff (other: Expr [A]): Expr [A] = Or (And (self, other), Not (Or (self, other)))

    def && (other: Expr [A]): Expr [A] = and (other)
    def || (other: Expr [A]): Expr [A] = or (other)
    def ^ (other: Expr [A]): Expr [A] = xor (other)
    def --> (other: Expr [A]): Expr [A] = implies (other)
    def <-- (other: Expr [A]): Expr [A] = impliedBy (other)
    def <--> (other: Expr [A]): Expr [A] = iff (other)
    def unary_! : Expr [A] = Not (self)
  }

  def cata [A, B] (
    expr: Expr [A],
    and: (B, B) => B,
    context: (String, B) => B,
    not: B => B,
    or: (B, B) => B,
    pure: A => B):
  B = {
    def f (expr: Expr [A]): B = expr match {
      case And (l, r) => and (f (l), f (r))
      case Context (c, e) => context (c, f (e))
      case Not (e) => not (f (e))
      case Or (l, r) => or (f (l), f (r))
      case Pure (a) => pure (a)
    }
    f (expr)
  }

  def cataS [S, A, B] (
    expr: Expr [A],
    state: S,
    and: S => (S, (B, B) => B),
    context: S => (S, (String, B) => B),
    not: S => (S, B => B),
    or: S => (S, (B, B) => B),
    pure: S => A => B):
  B = {
    def f (s: S, expr: Expr [A]): B = expr match {
      case And (l, r) => and (s) match { case (ss, g) => g (f (ss, l), f (ss, r)) }
      case Context (c, e) => context (s) match { case (ss, g) => g (c, f (ss, e)) }
      case Not (e) => not (s) match { case (ss, g) => g (f (ss, e)) }
      case Or (l, r) => or (s) match { case (ss, g) => g (f (ss, l), f (ss, r)) }
      case Pure (a) => pure (s) (a)
    }
    f (state, expr)
  }

  def run [A] (expr: Expr [A], p: Predicate [A]): Boolean =
    cata [A, Boolean] (expr, _ && _, (_, b) => b, ! _, _ || _, p)

  def prune [A, B] (expr: Expr [A], p: Predicate [A], f: A => B, keepWhen: Boolean): Option [Expr [B]] =
    cataS [Boolean, A, Option [Expr [B]]] (
      expr,
      keepWhen,
      s => (s, (if (s) pruneRequireBoth [B] _ else pruneRequireEither [B] _) (semigroupAnd .algebra [B]) .combine),
      s => (s, (c, e) => e map (Context (c, _))),
      s => (! s, _ map Not.apply),
      s => (s, (if (s) pruneRequireEither [B] _ else pruneRequireBoth [B] _) (semigroupOr .algebra [B]) .combine),
      s => a => if (p (a) ^ s) None else Some (Pure (f (a))))

  implicit def arbitrary [A] (implicit a: Arbitrary [A], s: Arbitrary [String]): Arbitrary [Expr [A]] =
    Arbitrary (
      Gen .frequency (
        (1, Gen delay (for { l <- arbitrary (a, s) .arbitrary; r <- arbitrary (a, s) .arbitrary } yield And (l, r))),
        (1, Gen delay (for { c <- s.arbitrary; e <- arbitrary (a, s) .arbitrary } yield Context (c, e))),
        (1, Gen delay (for { e <- arbitrary (a, s) .arbitrary } yield Not (e))),
        (1, Gen delay (for { l <- arbitrary (a, s) .arbitrary; r <- arbitrary (a, s) .arbitrary } yield Or (l, r))),
        (3, a.arbitrary map Pure [A])))

  object internal {
    type Expr [A] = BooleanExpression [A]

    def pruneRequireBoth [A] (combine: Semigroup [Expr [A]]): Semigroup [Option [Expr [A]]] =
      Apply semigroup (Apply [Option], combine)

    def pruneRequireEither [A] (combine: Semigroup [Expr [A]]): Semigroup [Option [Expr [A]]] =
      cats.instances.option catsKernelStdMonoidForOption combine

    @scala.annotation.tailrec
    def prettyDepth (sb: StringBuilder, depth: Int): StringBuilder =
      if (depth <= 0) sb else prettyDepth (sb append ' ', depth - 1)

    def prettyExpr [E] (expr: Expr [E], parent: Option [Expr [E]], sb: StringBuilder, depth: Int) (implicit show: Show [E]): StringBuilder =
      (expr, parent) match {
        case (And (a, b), Some (And (_, _))) =>
          prettyExpr (a, Some (expr), sb, depth)
          prettyExpr (b, Some (expr), sb, depth)
        case (And (a, b), _) =>
          prettyDepth (parent map (_ => sb append '\n') getOrElse sb, depth)
          sb append "[ allOf ]"
          prettyExpr (a, Some (expr), sb, depth + 2)
          prettyExpr (b, Some (expr), sb, depth + 2)
        case (Context (c, a), Some (Context (_, _))) =>
          sb append ' '
          sb append "[[ " append c append " ]]"
          prettyExpr (a, Some (expr), sb, depth)
        case (Context (c, a), _) =>
          prettyDepth (parent map (_ => sb append '\n') getOrElse sb, depth)
          sb append "[[ " append c append " ]]"
          prettyExpr (a, Some (expr), sb, depth + 2)
        case (Not (a), Some (Not (_))) =>
          sb append ' '
          sb append "[ not ]"
          prettyExpr (a, Some (expr), sb, depth)
        case (Not (a), _) =>
          prettyDepth (parent map (_ => sb append '\n') getOrElse sb, depth)
          sb append "[ not ]"
          prettyExpr (a, Some (expr), sb, depth + 2)
        case (Or (a, b), Some (Or (_, _))) =>
          prettyExpr (a, Some (expr), sb, depth)
          prettyExpr (b, Some (expr), sb, depth)
        case (Or (a, b), _) =>
          prettyDepth (parent map (_ => sb append '\n') getOrElse sb, depth)
          sb append "[ anyOf ]"
          prettyExpr (a, Some (expr), sb, depth + 2)
          prettyExpr (b, Some (expr), sb, depth + 2)
        case (Pure (a), Some (Not (_))) =>
          sb append ' '
          sb append (show show a)
        case (Pure (a), _) =>
          prettyDepth (parent map (_ => sb append '\n') getOrElse sb, depth)
          sb append (show show a)
      }
  }
}

package com.earnest.microtypical.data

import cats.{UnorderedFoldable, Show}
import cats.syntax.functor._
import com.earnest.microtypical.Predicate
import com.earnest.microtypical.data.BooleanExpression.monad.map
import com.earnest.microtypical.data.BooleanExpression.{Context, Pure, prune}
import com.earnest.microtypical.data.Bound.{Exclusive, Inclusive}
import com.earnest.microtypical.data.Constraint.profunctor.lmap

package object validation {
  type Constraints [A] = BooleanExpression [Constraint [A, Error]]
  type Error = String
  type Errors = BooleanExpression [Error]
  type ErrorsOr [A] = Either [Errors, A]

  def validate [V, R] (constraints: Constraints [R], f: R => V) (value: R): ErrorsOr [V] =
    (prune [Constraint [R, Error], Error] (
      constraints,
      _ predicate value,
      _.error,
      keepWhen = false)
    toLeft f (value))

  def constraint [A] (predicate: Predicate [A], error: Error): Constraints [A] =
    Pure (Constraint (predicate, error))

  def context [B, A] (context: String, f: B => A, constraints: Constraints [A]): Constraints [B] =
    Context (context, map (constraints) (lmap (_) (f)))

  def lt [A] (value: A) (implicit o: Ordering [A], s: Show [A]): Constraints [A] =
    constraint (o lt (_, value), s"be < ${s show value}")

  def lteq [A] (value: A) (implicit o: Ordering [A], s: Show [A]): Constraints [A] =
    constraint (o lteq (_, value), s"be <= ${s show value}")

  def gt [A] (value: A) (implicit o: Ordering [A], s: Show [A]): Constraints [A] =
    constraint (o gt (_, value), s"be > ${s show value}")

  def gteq [A] (value: A) (implicit o: Ordering [A], s: Show [A]): Constraints [A] =
    constraint (o gteq (_, value), s"be >= ${s show value}")

  def equiv [A] (value: A) (implicit o: Ordering [A], s: Show [A]): Constraints [A] =
    constraint (o equiv (_, value), s"be == ${s show value}")

  def equal [A] (value: A) (implicit s: Show [A]): Constraints [A] =
    constraint (_ == value, s"be ${s show value}")

  def oneOf [A] (values: Set [A]) (implicit s: Show [A]): Constraints [A] =
    constraint (values, s"be one of (${(values map s.show).toVector.sorted mkString ", "})")

  def max [A] (bound: Bound [A]) (implicit o: Ordering [A], s: Show [A]): Constraints [A] =
    bound match {
      case Exclusive (value) => lt (value) (o, s)
      case Inclusive (value) => lteq (value) (o, s)
    }

  def min [A] (bound: Bound [A]) (implicit o: Ordering [A], s: Show [A]): Constraints [A] =
    bound match {
      case Exclusive (value) => gt (value) (o, s)
      case Inclusive (value) => gteq (value) (o, s)
    }

  def bounded [A] (bounds: Bounds [A]) (implicit o: Ordering [A], s: Show [A]): Constraints [A] =
    min (bounds.min) (o, s) and max (bounds.max) (o, s)

  def boundedRepresentation [V, R] (bounds: Bounds [Representation [V, R]]) (implicit o: Ordering [R], s: Show [R]): Constraints [R] =
    min (bounds.min map (_.value)) (o, s) and max (bounds.max map (_.value)) (o, s)

  def defined [A]: Constraints [Option [A]] =
    constraint [Option [A]] (_.isDefined, "be defined")

  def empty [F [_], A] (implicit f: UnorderedFoldable [F]) =
    constraint [F [A]] (f.isEmpty, "be empty")

  def size [F [_], A] (constraints: Constraints [Long]) (implicit f: UnorderedFoldable [F]): Constraints [F [A]] =
    context ("size", f.size, constraints)
}

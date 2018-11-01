package com.earnest.microtypical.data

import cats.syntax.either._
import com.earnest.microtypical.Show

trait Model [V, R, E] {
  def apply (r: R): Either [E, V]
  def applyOrShow (r: R) (implicit s: Show [E]): Either [String, V] = apply (r) leftMap s.show
  def applyUnsafe (r: R) (implicit s: Show [E]): V = apply (r) fold (e => throw new IllegalArgumentException (s show e), identity)
  def preview (r: R): Option [V] = apply (r) .toOption
  def review (v: V): R
  def validated (r: R): cats.data.Validated [E, V] = cats.data.Validated fromEither apply (r)
  def validatedNel (r: R): cats.data.ValidatedNel [E, V] = validated (r) .toValidatedNel
}

object Model {
  def apply [V, R, E] (implicit m: Model [V, R, E]) = m

  def instance [V, R, E] (_apply: R => Either [E, V], _review: V => R): Model [V, R, E] =
    new Model [V, R, E] {
      override def apply (r: R): Either [E, V] = _apply (r)
      override def review (v: V): R = _review (v)
    }

  def subtype [A, B, E, R] (submodel: Model [A, R, E], supermodel: Model [B, R, E]) (implicit s: Show [E]): Model [A, B, E] =
    Model instance (
      supermodel.review _ andThen submodel.apply,
      submodel.review _ andThen supermodel.applyUnsafe)

  def transitive [A, B, C, E] (ab: Model [A, B, E], bc: Model [B, C, E]): Model [A, C, E] =
    Model instance (bc.apply _ andThen (_ flatMap ab.apply), ab.review _ andThen bc.review)

  def fromIso [A, B, R] (ab: Iso [A, B]): Model [A, B, R] =
    Model instance (ab.unapply andThen Right.apply, ab.apply)
}

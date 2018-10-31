package com.earnest.microtypical.data.collection

import cats.implicits.catsStdInstancesForSet
import cats.syntax.functor.toFunctorOps
import cats.syntax.profunctor.toProfunctorOps
import com.earnest.microtypical.data._
import com.earnest.microtypical.data.validation._
import org.scalacheck.{Arbitrary, Gen}

abstract sealed case class NonEmptyMap [K, V] (value: Map [K, V]) extends Validated
object NonEmptyMap extends ValidatedCompanion.Poly2 [NonEmptyMap, Map, Errors] {
  override implicit def model [K, V]: Model [NonEmptyMap [K, V], Map [K, V], Errors] =
    Model .instance (
      validate [NonEmptyMap [K, V], Map [K, V]] (
        ! empty [Set, K] map (_ lmap (_.keySet)),
        new NonEmptyMap [K, V] (_) {}),
      _.value)

  implicit def arbitrary [K, V] (implicit kv: Arbitrary [(K, V)]): Arbitrary [NonEmptyMap [K, V]] =
    Arbitrary (Gen .nonEmptyMap [K, V] (kv.arbitrary) map applyUnsafe)
}

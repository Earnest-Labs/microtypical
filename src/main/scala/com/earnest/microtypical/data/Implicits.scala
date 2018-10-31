package com.earnest.microtypical.data

import java.util.Comparator

import cats.syntax.functorFilter._
import cats.syntax.contravariant._
import cats.syntax.functor.toFunctorOps
import cats.{Order, Show}
import com.earnest.microtypical.instances.scalacheck.arbitrary.microtypicalInstancesScalacheckArbitraryMonadFilter
import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.encoding.DerivedObjectEncoder
import io.circe.{Decoder, Encoder}
import org.scalacheck.Gen.Choose
import org.scalacheck.{Arbitrary, Gen}
import shapeless.Lazy

trait Implicits extends ImplicitsOrder1

trait ImplicitsOrder1 extends ImplicitsOrder2 {
  implicit def microtypicalDataOrderFromOrdering [V] (implicit o: Ordering [V]): Order [V] = Order fromOrdering o
}

trait ImplicitsOrder2 extends ImplicitsEnumerated {
  implicit def microtypicalDataOrderFromComparator [V] (implicit c: Comparator [V]): Order [V] = Order from c.compare
}

trait ImplicitsEnumerated extends ImplicitsIso1 {
  implicit def microtypicalDataArbitraryFromEnumerated [V] (implicit e: Enumerated [V]): Arbitrary [V] =
    Arbitrary (Gen oneOf e.values.toVector)
}

trait ImplicitsIso1 extends ImplicitsIso2 {
  implicit def microtypicalDataArbitraryFromIso [V, R] (implicit i: Iso [V, R], a: Arbitrary [R]): Arbitrary [V] =
    a map i.unapply

  implicit def microtypicalDataBoundsFromIso [V, R] (implicit i: Iso [V, R], b: Bounds [R]): Bounds [V] =
    b map i.unapply

  implicit def microtypicalDataChooseFromIso [V, R] (implicit i: Iso [V, R], c: Choose [R]): Choose [V] =
    Choose .xmap (i.unapply, i.apply) (c)

  implicit def microtypicalDataComparatorFromIso [V, R] (implicit i: Iso [V, R], c: Comparator [R]): Comparator [V] =
    (l: V, r: V) => c compare (i apply l, i apply r)

  implicit def microtypicalDataDecoderFromIso [V, R, E] (implicit i: Iso [V, R], d: Decoder [R]): Decoder [V] =
    d map i.unapply

  implicit def microtypicalDataEncoderFromIso [V, R] (implicit i: Iso [V, R], e: Encoder [R]): Encoder [V] =
    e contramap i.apply

  implicit def microtypicalDataShowFromIso [V, R] (implicit i: Iso [V, R], s: Show [R]): Show [V] =
    s contramap i.apply
}

trait ImplicitsIso2 extends ImplicitsModel1 {
  implicit def microtypicalDataDecoderFromIsoDerived [V, R, E] (implicit i: Iso [V, R], d: Lazy [DerivedDecoder [R]]): Decoder [V] =
    d.value map i.unapply

  implicit def microtypicalDataEncoderFromIsoDerived [V, R] (implicit i: Iso [V, R], e: Lazy [DerivedObjectEncoder [R]]): Encoder [V] =
    e.value contramap i.apply
}

trait ImplicitsModel1 extends ImplicitsModel2 {
  implicit def microtypicalDataChoose [V, R] (implicit m: Model [V, R, _], g: GapFree [V], c: Choose [R]): Choose [V] =
    Choose .xmap (m.applyUnsafe, m.review) (c)

  implicit def microtypicalDataChooseRepresentation [V, R] (implicit m: Model [V, R, _], g: GapFree [V], c: Choose [R]): Choose [Representation [V, R]] =
    Choose .xmap [R, Representation [V, R]] (Representation.apply, _.value) (c)

  implicit def microtypicalDataComparator [V, R] (implicit m: Model [V, R, _], c: Comparator [R]): Comparator [V] =
    (l: V, r: V) => c compare (m review l, m review r)

  implicit def microtypicalDataDecoder [V, R, E] (implicit m: Model [V, R, E], d: Decoder [R], s: Show [E]): Decoder [V] =
    d emap m.applyOrShow

  implicit def microtypicalDataEncoder [V, R] (implicit m: Model [V, R, _], e: Encoder [R]): Encoder [V] =
    e contramap m.review

  implicit def microtypicalDataShow [V, R] (implicit m: Model [V, R, _], s: Show [R]): Show [V] =
    s contramap m.review
}

trait ImplicitsModel2 extends ImplicitsModel3 {
  implicit def microtypicalDataArbitraryLow [V] (implicit b: Bounds [V], c: Choose [V], o: Ordering [V]): Arbitrary [V] =
    Arbitrary (Gen choose (b.min.value, b.max.value) filter (Bounds contains b))

  implicit def microtypicalDataComparatorLow [V, R] (implicit m: Model [V, R, _], o: Ordering [R]): Comparator [V] =
    (l: V, r: V) => o compare (m review l, m review r)

  implicit def microtypicalDataDecoderDerived [V, R, E] (implicit m: Model [V, R, E], d: Lazy [DerivedDecoder [R]], s: Show [E]): Decoder [V] =
    d.value emap m.applyOrShow

  implicit def microtypicalDataEncoderDerived [V, R] (implicit m: Model [V, R, _], e: Lazy [DerivedObjectEncoder [R]]): Encoder [V] =
    e.value contramap m.review
}

trait ImplicitsModel3 {
  implicit def microtypicalDataArbitraryLow1 [V, R] (implicit b: Bounds [Representation [V, R]], c: Choose [Representation [V, R]], m: Model [V, R, _], o: Ordering [Representation [V, R]]): Arbitrary [V] =
    Arbitrary (Gen choose (b.min.value, b.max.value) filter (Bounds contains b)) mapFilter (m preview _.value)
}

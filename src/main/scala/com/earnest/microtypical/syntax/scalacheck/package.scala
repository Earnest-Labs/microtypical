package com.earnest.microtypical.syntax

import _root_.cats.data.{OneAnd, NonEmptyList => Nel, NonEmptyVector => Nev}
import com.earnest.microtypical.data.implicits.microtypicalDataArbitraryLow
import com.earnest.microtypical.data.numeric.{Gt0, Gteq0}
import com.earnest.microtypical.data.text.CodePoint
import com.earnest.microtypical.data.{Bound, Bounds}
import org.scalacheck.Arbitrary.{arbitrary => arb}
import org.scalacheck.Gen.Choose
import org.scalacheck.{Arbitrary, Gen}

import _root_.scala.annotation.tailrec
import _root_.scala.util.Random

package object scalacheck {

  implicit def arbOneAnd [F [_], A] (implicit arbA: Arbitrary [A], arbF: Arbitrary [F [A]]): Arbitrary [OneAnd [F, A]] =
    Arbitrary (for {
      h <- arbA.arbitrary
      t <- arbF.arbitrary
    } yield OneAnd (h, t))

  implicit def arbNel [A] (implicit a: Arbitrary [A]): Arbitrary [Nel [A]] =
    Arbitrary (Gen nonEmptyListOf a.arbitrary map Nel.fromListUnsafe)

  implicit def arbNev [A] (implicit a: Arbitrary [A]): Arbitrary [Nev [A]] =
    Arbitrary (Gen .nonEmptyContainerOf [Vector, A] (a.arbitrary) map Nev.fromVectorUnsafe)

  implicit def arbRandom: Arbitrary [Random] = Arbitrary (arb [Uniform [Long]] map (v => new Random (v.value)))

  def genSomeWhen [A] (gen: Gen [A], b: Boolean): Gen [Option [A]] = if (b) gen map Some.apply else Gen option gen

  def genSomeWhenSome [A, B] (gen: Gen [A], b: Option [B]): Gen [Option [A]] = genSomeWhen (gen, b.isDefined)

  def genSomeWhenNone [A, B] (gen: Gen [A], b: Option [B]): Gen [Option [A]] = genSomeWhen (gen, b.isEmpty)

  def genSomeIff [A] (gen: Gen [A], b: Boolean): Gen [Option [A]] = if (b) gen map Some.apply else Gen const None

  def genSomeIffSome [A, B] (gen: Gen [A], b: Option [B]): Gen [Option [A]] = genSomeIff (gen, b.isDefined)

  def genSomeIffNone [A, B] (gen: Gen [A], b: Option [B]): Gen [Option [A]] = genSomeIff (gen, b.isEmpty)

  def genNoneWhen [A] (gen: Gen [A], b: Boolean): Gen [Option [A]] = if (b) Gen const None else Gen option gen

  def genNoneWhenSome [A, B] (gen: Gen [A], b: Option [B]): Gen [Option [A]] = genNoneWhen (gen, b.isDefined)

  def genNoneWhenNone [A, B] (gen: Gen [A], b: Option [B]): Gen [Option [A]] = genNoneWhen (gen, b.isEmpty)

  def genDecimalGteq0 (max: Long): Gen [Long] = for {
    maxDigits <- Gen choose (0, max .toString .length)
    maxValue = Math min (max, (1 to maxDigits) .foldLeft (0L) ((acc, _) => 10 * acc + 9))
    value <- Gen choose (0L, maxValue)
  } yield value

  def genDecimalPlusMinus (maxMagnitude: Long): Gen [Long] = for {
    magnitude <- genDecimalGteq0 (maxMagnitude)
    sign <- arb [Boolean]
  } yield if (sign) magnitude else -magnitude

  def genNelOfN [A] (n: Gt0 [Int], a: Gen [A]): Gen [Nel [A]] = Gen listOfN (n.value, a) map Nel.fromListUnsafe

  def genNevOfN [A] (n: Gt0 [Int], a: Gen [A]): Gen [Nev [A]] = Gen listOfN (n.value, a) map (Nev fromVectorUnsafe _.toVector)

  def genStringOfN (n: Gteq0 [Int], includeNull: Boolean): Gen [String] =
    (Gen listOfN (
      n.value,
      CodePoint.uniform.arbitrary
        map (_.value)
        filter (includeNull || _ != CodePoint.NULL))
      map (nel => (nel.iterator map CodePoint.show) .mkString))

  @tailrec
  def one [A] (gen: Gen [A], tries: Int = 100): A =
    if (tries <= 0)
      throw new IllegalStateException ("Unable to find an arbitrary value for: " + gen)
    else gen.sample match {
      case Some (a) => a
      case None => one (gen, tries - 1)
    }

  def genInside [A] (b: Bounds [A]) (implicit c: Choose [A], o: Ordering [A]): Gen [A] =
    microtypicalDataArbitraryLow (b, c, o) .arbitrary

  def genOutside [A] (inner: Bounds [A]) (implicit b: Bounds [A], c: Choose [A], o: Ordering [A]): Gen [A] =
    Gen frequency (
      (List (
        Bounds (b.min, Bound flip inner.min),
        Bounds (Bound flip inner.max, b.max))
      map (b => (if (Bounds canProveEmpty b) 0 else 1, genInside (b)))): _*)
}

package com.earnest.microtypical.syntax.scalacheck

import com.earnest.microtypical.data.numeric.Gteq0
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

import scala.annotation.tailrec

class ScalaCheckTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  type T = Byte

  val someAndNoneTries: Gteq0 [Int] = Gteq0 applyUnsafe 1000

  def shouldProduceSome [A] (g: Gen [Option [A]]) = g.sample.get shouldBe a [Some [_]]

  def shouldProduceNone [A] (g: Gen [Option [A]]) = g.sample.get shouldBe None

  def shouldProduceBoth [A] (g: Gen [Option [A]]) = {
    @tailrec def f (foundSome: Boolean, foundNone: Boolean, tries: Gteq0 [Int]): Unit =
      if (foundSome && foundNone) ()
      else Gteq0 preview (tries.value - 1) match {
        case None => fail (s"foundSome = ${foundSome}, foundNone = ${foundNone}")
        case Some (remaining) =>
          val next = g.sample.get
          f (foundSome || next.isDefined, foundNone || next.isEmpty, remaining)
      }

    f (false, false, someAndNoneTries)
  }

  "An arbitrary DistinctPair [_] should had distinct elements" in forAll (
    (p: DistinctPair [T]) => p._1 should not be p._2)

  // genSomeWhen

  "genSomeWhen (_, true) should generate Some" in forAll (
    (g: Gen [T]) => shouldProduceSome (genSomeWhen (g, true)))

  "genSomeWhen (_, false) should generate both" in forAll (
    (g: Gen [T]) => shouldProduceBoth (genSomeWhen (g, false)))

  "genSomeWhenSome (_, Some (_)) should generate Some" in forAll (
    (g: Gen [T], v: T) => shouldProduceSome (genSomeWhenSome (g, Some (v))))

  "genSomeWhenSome (_, None) should generate both" in forAll (
    (g: Gen [T]) => shouldProduceBoth (genSomeWhenSome (g, None)))

  "genSomeWhenNone (_, Some (_)) should generate both" in forAll (
    (g: Gen [T], v: T) => shouldProduceBoth (genSomeWhenNone (g, Some (v))))

  "genSomeWhenNone (_, None) should generate Some" in forAll (
    (g: Gen [T]) => shouldProduceSome (genSomeWhenNone (g, None)))

  // genSomeIff

  "genSomeIff (_, true) should generate Some" in forAll (
    (g: Gen [T]) => shouldProduceSome (genSomeIff (g, true)))

  "genSomeIff (_, false) should generate None" in forAll (
    (g: Gen [T]) => shouldProduceNone (genSomeIff (g, false)))

  "genSomeIffSome (_, Some (_)) should generate Some" in forAll (
    (g: Gen [T], v: T) => shouldProduceSome (genSomeIffSome (g, Some (v))))

  "genSomeIffSome (_, None) should generate None" in forAll (
    (g: Gen [T]) => shouldProduceNone (genSomeIffSome (g, None)))

  "genSomeIffNone (_, Some (_)) should generate None" in forAll (
    (g: Gen [T], v: T) => shouldProduceNone (genSomeIffNone (g, Some (v))))

  "genSomeIffNone (_, None) should generate Some" in forAll (
    (g: Gen [T]) => shouldProduceSome (genSomeIffNone (g, None)))

  // genNoneWhen

  "genNoneWhen (_, true) should generate None" in forAll (
    (g: Gen [T]) => shouldProduceNone (genNoneWhen (g, true)))

  "genNoneWhen (_, false) should generate both" in forAll (
    (g: Gen [T]) => shouldProduceBoth (genNoneWhen (g, false)))

  "genNoneWhenSome (_, Some (_)) should generate None" in forAll (
    (g: Gen [T], v: T) => shouldProduceNone (genNoneWhenSome (g, Some (v))))

  "genNoneWhenSome (_, None) should generate both" in forAll (
    (g: Gen [T]) => shouldProduceBoth (genNoneWhenSome (g, None)))

  "genNoneWhenNone (_, Some (_)) should generate both" in forAll (
    (g: Gen [T], v: T) => shouldProduceBoth (genNoneWhenNone (g, Some (v))))

  "genNoneWhenNone (_, None) should generate None" in forAll (
    (g: Gen [T]) => shouldProduceNone (genNoneWhenNone (g, None)))
}

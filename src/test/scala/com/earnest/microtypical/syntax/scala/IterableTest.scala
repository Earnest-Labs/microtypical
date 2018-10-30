package com.earnest.microtypical.syntax.scala

import com.earnest.microtypical.syntax.scala.iterable.toIterableOps
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

import scala.util.Random

final class IterableTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  type A = Byte
  type C = Vector [A]

  "isDistinct should return false if the collection contains a duplicate" in forAll (
    (c: C, a: A, s: Long) => (new Random (s) shuffle (c :+ a :+ a)) .isDistinct shouldBe false)

  "isDistinct should return true for sets" in forAll (
    (c: C) => c.toSet.isDistinct shouldBe true)
}

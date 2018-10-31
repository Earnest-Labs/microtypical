package com.earnest.microtypical.data.decimal

import com.earnest.microtypical.data.numeric.Gteq0
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.data.{Bounds, Model, Validated, ValidatedCompanion}
import com.earnest.microtypical.syntax.scalacheck.genInside
import org.scalacheck.Gen.Choose
import org.scalacheck.Gen.Choose.IllegalBoundsError
import org.scalacheck.{Arbitrary, Gen}
import shapeless.Succ
import shapeless.ops.nat.ToInt

abstract sealed case class Count [N] (value: Int) extends Validated

object Count extends ValidatedCompanion.Parametrized [Count, Int, ToInt, Succ [_], Errors] {
  def bounds [N <: Succ [_]] (implicit n: ToInt [N]): Bounds [Int] =
    Bounds inclusive (0, (Iterator continually 10 take n.apply).product - 1)

  override implicit def model [N <: Succ [_]] (implicit n: ToInt [N]): Model [Count [N], Int, Errors] =
    Model.instance [Count [N], Int, Errors] (
      validate (
        bounded (bounds (n)),
        new Count [N] (_: Int) {}),
      _.value)

  implicit def arbitrary [N <: Succ [_]] (implicit n: ToInt [N]): Arbitrary [Count [N]] =
    Arbitrary (genInside (bounds [N]) map model.applyUnsafe)

  implicit def choose [N <: Succ [_]] (implicit n: ToInt [N]): Choose [Count [N]] =
    (min, max) => {
      val (l, h) = (min.value, max.value)

      if (l > h) throw new IllegalBoundsError (min, max)
      else if (l == h) Gen const min
      else Gen choose (l, h) map model.applyUnsafe
    }

  def toGteq0 [N <: Succ [_]] (counter: Count [N]): Gteq0 [Int] =
    Gteq0 applyUnsafe counter.value
}

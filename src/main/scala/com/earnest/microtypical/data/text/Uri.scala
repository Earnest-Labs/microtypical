package com.earnest.microtypical.data.text

import cats.syntax.functor.toFunctorOps
import com.earnest.microtypical.data._
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.instances.scalacheck.arbitrary.microtypicalInstancesScalacheckArbitraryMonadFilter
import org.scalacheck.Arbitrary

abstract sealed case class Uri (value: String) extends Validated

object Uri extends ValidatedCompanion [Uri, String, Errors] {
  val constraints: Constraints [String] = NonEmptyString.constraints

  override implicit val model: Model [Uri, String, Errors] =
    Model .instance (validate [Uri, String] (constraints, new Uri (_) {}), _.value)

  val iso: Iso [Uri, NonEmptyString] =
    Iso.instance (
      NonEmptyString applyUnsafe _.value,
      Uri applyUnsafe _.value)

  implicit def arbitrary (implicit codePoint: Arbitrary [CodePoint]): Arbitrary [Uri] =
    NonEmptyString .arbitrary (codePoint) map (nes => applyUnsafe (nes.value))
}

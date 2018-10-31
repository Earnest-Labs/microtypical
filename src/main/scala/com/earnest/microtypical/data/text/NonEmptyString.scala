package com.earnest.microtypical.data.text

import cats.syntax.functor.toFunctorOps
import cats.syntax.functorFilter._
import com.earnest.microtypical.data._
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.instances.scalacheck.arbitrary.microtypicalInstancesScalacheckArbitraryMonadFilter
import com.earnest.microtypical.syntax.scalacheck.arbNel
import org.scalacheck.Arbitrary

abstract sealed case class NonEmptyString (value: String) extends Validated

/**
  * Example:
  * {{{
  * scala> Nes.applyUnsafe("absolutely")
  * res0: com.earnest.microtypical.data.text.NonEmptyString = NonEmptyString(absolutely)
  * }}}
  */
object NonEmptyString extends ValidatedCompanion [NonEmptyString, String, Errors] {
  val constraints: Constraints [String] =
    constraint [String] (_.length > 0, "not be empty") and
    constraint [String] (_ .indexOf ('\u0000') == -1, "not contain the NULL character")

  override implicit val model: Model [NonEmptyString, String, Errors] =
    Model .instance (validate [NonEmptyString, String] (constraints, new NonEmptyString (_) {}), _.value)

  implicit def arbitrary (implicit codePoint: Arbitrary [CodePoint]): Arbitrary [NonEmptyString] =
    (arbNel (codePoint filter (_ != CodePoint.NULL))
      map (nel => (nel.toList.iterator map CodePoint.show) .mkString)
      map model.applyUnsafe)
}

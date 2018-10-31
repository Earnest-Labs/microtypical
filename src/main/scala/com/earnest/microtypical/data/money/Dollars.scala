package com.earnest.microtypical.data.money

import cats.syntax.functor._
import com.earnest.microtypical.data.implicits.microtypicalDataChooseFromIso
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.data.{Bounds, Iso, Model, Validated, ValidatedCompanion}
import org.scalacheck.Gen.Choose

abstract sealed case class Dollars (value: BigDecimal) extends Validated

object Dollars extends ValidatedCompanion [Dollars, BigDecimal, Errors] {
  implicit val bounds: Bounds [Dollars] =
    Bounds inclusive (minDollars, maxDollars) map (new Dollars (_) {})

  val constraints: Constraints [BigDecimal] =
    bounded (bounds map (_.value)) and
    context ("scale", _.scale, lteq (2))

  override implicit val model: Model [Dollars, BigDecimal, Errors] =
    Model .instance (validate (constraints, new Dollars (_: BigDecimal) {}), _.value)

  val fromCents: Iso [Cents, Dollars] =
    Iso .instance (
      c => model applyUnsafe (centsToDollars * c.value),
      d => Cents.model applyUnsafe (dollarsToCents * d.value).toLongExact)

  implicit def choose: Choose [Dollars] = microtypicalDataChooseFromIso [Dollars, Cents] (Iso symmetric fromCents, implicitly)
}

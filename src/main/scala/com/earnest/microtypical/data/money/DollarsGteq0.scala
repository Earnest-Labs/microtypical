package com.earnest.microtypical.data.money

import cats.instances.bigDecimal.catsStdShowForBigDecimal
import cats.instances.int.catsStdShowForInt
import cats.syntax.functor._
import com.earnest.microtypical.data.implicits.microtypicalDataChooseFromIso
import com.earnest.microtypical.data.validation._
import com.earnest.microtypical.data.{Bounds, Iso, Model, Validated, ValidatedCompanion}
import org.scalacheck.Gen.Choose

abstract sealed case class DollarsGteq0 (value: BigDecimal) extends Validated

object DollarsGteq0 extends ValidatedCompanion [DollarsGteq0, BigDecimal, Errors] {
  implicit val bounds: Bounds [DollarsGteq0] =
    Bounds inclusive (BigDecimal (0), maxDollars) map (new DollarsGteq0 (_) {})

  val constraints: Constraints [BigDecimal] =
    bounded (bounds map (_.value)) and
    context ("scale", _.scale, lteq (2))

  override implicit val model: Model [DollarsGteq0, BigDecimal, Errors] =
    Model .instance (validate (constraints, new DollarsGteq0 (_: BigDecimal) {}), _.value)

  val fromCentsGteq0: Iso [CentsGteq0, DollarsGteq0] =
    Iso .instance (
      c => model applyUnsafe (centsToDollars * c.value),
      d => CentsGteq0.model applyUnsafe (dollarsToCents * d.value).toLongExact)

  val fromDollars: Model [DollarsGteq0, Dollars, Errors] = Model subtype (model, Dollars.model)

  implicit def choose: Choose [DollarsGteq0] =
    microtypicalDataChooseFromIso [DollarsGteq0, CentsGteq0] (Iso symmetric fromCentsGteq0, implicitly)
}

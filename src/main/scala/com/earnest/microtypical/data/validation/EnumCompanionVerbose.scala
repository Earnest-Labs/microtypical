package com.earnest.microtypical.data.validation

import cats.instances.int.catsStdShowForInt
import cats.instances.string.catsStdShowForString
import com.earnest.microtypical.data.collection.NonEmptyMap
import com.earnest.microtypical.data.{Enumerated, Model, ValidatedCompanion}

trait EnumCompanionVerbose [V] extends ValidatedCompanion [V, String, Errors] {
  protected def encoding: Map [V, String]

  private val encode = encoding
  private val decode = encode map (_.swap)

  implicit val enumerated: Enumerated [V] = Enumerated instance encode.keySet

  val constraints: Constraints [String] = oneOf (decode.keySet)

  override implicit val model: Model [V, String, Errors] = Model instance (validate (constraints, decode.apply), encode.apply)
}

object EnumCompanionVerbose {
  trait Versioned [V] extends ValidatedCompanion [V, Versioned.Repr, Errors] {
    import Versioned.Repr

    protected def encoding: NonEmptyMap [V, Repr]

    private val encode: Map [V, Repr] = encoding.value
    private val decode: Map [Repr, V] = encode map (_.swap)

    implicit lazy val enumerated: Enumerated [V] = Enumerated instance encode.keySet

    val constraints: Constraints [Repr] =
      decode.keySet .
        groupBy (_.version) .
        toVector .
        sortBy (_._1) .
        map { case (version, reprs) =>
          context [Repr, Int] ("version", _.version, equal (version)) and
          context [Repr, String] ("value", _.value, oneOf (reprs map (_.value)))
        } .
        reduce (_ || _)

    override implicit val model: Model [V, Repr, Errors] =
      Model instance (validate (constraints, decode.apply), encode.apply)
  }

  object Versioned {
    case class Repr (version: Int, value: String)

    def encoding [A <: B, B] (version: Int, ec: EnumCompanionVerbose [A]): Map [B, Repr] =
      ec.encode map { case (a, s) => (a: B, Repr (version, s)) }
  }
}

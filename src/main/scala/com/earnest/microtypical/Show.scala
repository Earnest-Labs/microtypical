package com.earnest.microtypical

import java.time.{Instant, LocalDate, Year, YearMonth}

import cats.Contravariant

import scala.language.implicitConversions

//
// Copied from cats.Show to avoid implicit import tax and confusion
//

/**
 * A type class to provide textual representation. It is meant to be a
 * better "toString". Whereas toString exists for any Object,
 * regardless of whether or not the creator of the class explicitly
 * made a toString method, a Show instance will only exist if someone
 * explicitly provided one.
 */
trait Show[T] extends Show.ContravariantShow[T]

/**
 * Hand rolling the type class boilerplate due to scala/bug#6260 and scala/bug#10458
 */
object Show extends Show2 {

  def apply[A](implicit instance: Show[A]): Show[A] = instance

  trait ContravariantShow[-T] extends Serializable {
    def show(t: T): String
  }

  trait Ops[A] {
    def typeClassInstance: Show[A]
    def self: A
    def show: String = typeClassInstance.show(self)
  }

  trait ToShowOps {
    implicit def toShow[A](target: A)(implicit tc: Show[A]): Ops[A] = new Ops[A] {
      val self = target
      val typeClassInstance = tc
    }
  }

  /** creates an instance of [[Show]] using the provided function */
  def show[A](f: A => String): Show[A] = new Show[A] {
    def show(a: A): String = f(a)
  }

  /** creates an instance of [[Show]] using object toString */
  def fromToString[A]: Show[A] = new Show[A] {
    def show(a: A): String = a.toString
  }

  final case class Shown(override val toString: String) extends AnyVal
  object Shown {
    implicit def mat[A](x: A)(implicit z: ContravariantShow[A]): Shown = Shown(z show x)
  }

  final case class ShowInterpolator(_sc: StringContext) extends AnyVal {
    def show(args: Shown*): String = _sc s (args: _*)
  }

  //
  // Microtypical changes start here
  //

  implicit val microtypicalContravariantForShow: Contravariant[Show] = new Contravariant[Show] {
    def contramap[A, B](fa: Show[A])(f: B => A): Show[B] =
      show[B](fa.show _ compose f)
  }

  object toCats {
    implicit def microtypicalShowToCatsShow [A] (implicit s: Show [A]): cats.Show [A] = cats.Show show s.show
    implicit def microtypicalShowToCatsShowContravariant [A] (implicit s: Show.ContravariantShow [A]): cats.Show.ContravariantShow [A] = cats.Show show s.show
  }

  implicit def fromCats [A] (implicit catsShow: cats.Show [A]): Show [A] = show (catsShow.show)

  implicit val string: Show [String] = show (identity)

  implicit val boolean: Show [Boolean] = fromToString

  implicit val char: Show [Char] = fromToString

  implicit val byte: Show [Byte] = fromToString
  implicit val short: Show [Short] = fromToString
  implicit val int: Show [Int] = fromToString
  implicit val long: Show [Long] = fromToString

  implicit val float: Show [Float] = fromToString
  implicit val double: Show [Double] = fromToString

  implicit val bigInt: Show [BigInt] = fromToString
  implicit val bigDecimal: Show [BigDecimal] = fromToString

  implicit val instant: Show [Instant] = fromToString
  implicit val localDate: Show [LocalDate] = fromToString
  implicit val year: Show [Year] = fromToString
  implicit val yearMonth: Show [YearMonth] = fromToString
}

trait Show2 {
  import Show.ContravariantShow

  implicit def fromCatsContravariant [A] (implicit catsShow: cats.Show.ContravariantShow [A]): ContravariantShow [A] = Show show catsShow.show
}

package com.earnest.microtypical

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class ShowTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  type A = Object

  implicit val arbA: Arbitrary [A] = Arbitrary (Gen delay (Gen const new A {}))

  "Show" - {
    "implicit conversion from cats.Show should work" in forAll { a: A =>
      implicit val s: cats.Show [A] = cats.Show.fromToString

      (s show a) shouldBe (implicitly [Show [A]] show a)
    }

    "implicit conversion to cats.Show should work" in forAll { a: A =>
      import Show.toCats._

      implicit val s: Show [A] = Show.fromToString

      (s show a) shouldBe (implicitly [cats.Show [A]] show a)
    }

    "implicit conversion to cats.ContravariantShow should work" in forAll { a: A =>
      import Show.toCats._

      implicit val s: Show [A] = Show.fromToString

      (s show a) shouldBe (implicitly [cats.Show.ContravariantShow [A]] show a)
    }
  }

  "ContravariantShow" - {
    "implicit conversion from cats.Show should work" in forAll { a: A =>
      implicit val s: cats.Show [A] = cats.Show.fromToString

      (s show a) shouldBe (implicitly [Show.ContravariantShow [A]] show a)
    }

    "implicit conversion from cats.Show.ContravariantShow should work" in forAll { a: A =>
      implicit val s: cats.Show.ContravariantShow [A] = cats.Show.fromToString

      (s show a) shouldBe (implicitly [Show.ContravariantShow [A]] show a)
    }

    "implicit conversion to cats.ContravariantShow should work" in forAll { a: A =>
      import Show.toCats._

      implicit val s: Show.ContravariantShow [A] = Show.fromToString

      (s show a) shouldBe (implicitly [cats.Show.ContravariantShow [A]] show a)
    }
  }
}

package com.earnest.microtypical.data

import com.earnest.microtypical.Predicate
import com.earnest.microtypical.data.BooleanExpression.{Context, Pure, prune}
import com.earnest.microtypical.data.BooleanExpressionTest._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object BooleanExpressionTest {
  type T = Boolean
  type E = BooleanExpression [T]
  type P = Predicate [T]
  type U = T
  type F = T => U
}

class BooleanExpressionTest extends FreeSpec with GeneratorDrivenPropertyChecks {
  private def run (expr: E, p: P) = BooleanExpression run (expr, p)

  "run" - {
    """run (Pure (v), p) should be p (v)""" in forAll ((v: T, p: P) =>
      run (Pure (v), p) shouldBe p (v))

    """run (Context (s, a), p) should be run (a, p)""" in forAll ((a: E, s: String, p: P) =>
      run (Context (s, a), p) shouldBe run (a, p))

    """run (! a, p) should be ! run (a, p)""" in forAll ((a: E, p: P) =>
      run (! a, p) shouldBe ! run (a, p))

    """run (a && b, p) should be run (a, p) && run (b, p)""" in forAll ((a: E, b: E, p: P) =>
      run (a && b, p) shouldBe run (a, p) && run (b, p))

    """run (a || b, p) should be run (a, p) || run (b, p)""" in forAll ((a: E, b: E, p: P) =>
      run (a || b, p) shouldBe run (a, p) || run (b, p))

    """run (a ^ b, p) should be run (a, p) ^ run (b, p)""" in forAll ((a: E, b: E, p: P) =>
      run (a ^ b, p) shouldBe run (a, p) ^ run (b, p))

    """run (a --> b, p) should be run (! a || b, p)""" in forAll ((a: E, b: E, p: P) =>
      run (a --> b, p) shouldBe run (! a || b, p))

    """run (a <-- b, p) should be run (b --> a, p)""" in forAll ((a: E, b: E, p: P) =>
      run (a <-- b, p) shouldBe run (b --> a, p))

    """run (a <--> b, p) should be run ((a --> b) && (a <-- b), p)""" in forAll ((a: E, b: E, p: P) =>
      run (a <--> b, p) shouldBe run ((a --> b) && (a <-- b), p))
  }

  "prune" - {
    """prune (Pure (v), p, f, k) should be Some (Pure (f (v))) filter (_ => p (v) == k)""" in forAll ((v: T, p: P, f: F, k: Boolean) =>
      prune (Pure (v), p, f, k) shouldBe (Some (Pure (f (v))) filter (_ => p (v) == k)))

    """prune (Context (s, a), p, f, k) should be prune (a, p, f, k) map (Context (s, _))""" in forAll ((a: E, s: String, p: P, f: F, k: Boolean) =>
      prune (Context (s, a), p, f, k) shouldBe (prune (a, p, f, k) map (Context (s, _))))

    """prune (! a, p, f, k) should be prune (a, p, f, ! k) map !""" in forAll ((a: E, p: P, f: F, k: Boolean) =>
      prune (! a, p, f, k) shouldBe (prune (a, p, f, ! k) map (! _)))

    """prune (a && b, p, f, true) should require both branches""" in forAll ((a: E, b: E, p: P, f: F) =>
      prune (a && b, p, f, true) shouldBe
        ((prune (a, p, f, true), prune (b, p, f, true)) match {
          case (Some (ap), Some (bp)) => Some (ap && bp)
          case _ => None
        }))

    """prune (a && b, p, f, false) should keep all branches""" in forAll ((a: E, b: E, p: P, f: F) =>
      prune (a && b, p, f, false) shouldBe
        ((prune (a, p, f, false), prune (b, p, f, false)) match {
          case (Some (ap), Some (bp)) => Some (ap && bp)
          case (Some (ap), _) => Some (ap)
          case (_, Some (bp)) => Some (bp)
          case _ => None
        }))

    """prune (a || b, p, f, true) should keep all branches""" in forAll ((a: E, b: E, p: P, f: F) =>
      prune (a || b, p, f, true) shouldBe
        ((prune (a, p, f, true), prune (b, p, f, true)) match {
          case (Some (ap), Some (bp)) => Some (ap || bp)
          case (Some (ap), _) => Some (ap)
          case (_, Some (bp)) => Some (bp)
          case _ => None
        }))

    """prune (a || b, p, f, false) should require both branches""" in forAll ((a: E, b: E, p: P, f: F) =>
      prune (a || b, p, f, false) shouldBe
        ((prune (a, p, f, false), prune (b, p, f, false)) match {
          case (Some (ap), Some (bp)) => Some (ap || bp)
          case _ => None
        }))
  }
}

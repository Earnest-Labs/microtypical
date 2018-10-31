package com.earnest.microtypical.instances.scalacheck

import cats.{Functor, FunctorFilter, Monad}
import org.scalacheck.{Arbitrary, Gen}

trait arbitrary {
  implicit val microtypicalInstancesScalacheckArbitraryMonadFilter: Monad [Arbitrary] with FunctorFilter [Arbitrary] =
    new Monad [Arbitrary] with FunctorFilter [Arbitrary] {
      override val functor: Functor [Arbitrary] = this

      override def collect [A, B] (fa: Arbitrary[A]) (f: PartialFunction [A, B]): Arbitrary[B] =
        flattenOption (map (fa) (f.lift))

      override def filter [A] (fa: Arbitrary [A]) (f: A => Boolean) =
        Arbitrary (fa.arbitrary filter f)

      override def flatMap [A, B] (fa: Arbitrary [A]) (f: (A) => Arbitrary [B]): Arbitrary [B] =
        Arbitrary (fa.arbitrary flatMap (f andThen (_.arbitrary)))

      override def flattenOption [A] (fa: Arbitrary [Option [A]]): Arbitrary [A] = flatMap (fa) {
        case Some (i) => pure (i)
        case None => Arbitrary (Gen.fail)
      }

      override def map [A, B] (fa: Arbitrary [A]) (f: A => B) =
        Arbitrary (fa.arbitrary map f)

      override def mapFilter [A, B] (fa: Arbitrary [A]) (f: A => Option [B]): Arbitrary [B] =
        flattenOption (map (fa) (f))

      override def pure [A] (a: A): Arbitrary [A] =
        Arbitrary (Gen const a)

      override def tailRecM [A, B] (a: A) (f: (A) => Arbitrary [Either [A, B]]): Arbitrary [B] =
        flatMap (f (a)) {
          case Right (b) => pure (b)
          case Left (nextA) => tailRecM (nextA) (f)
        }
    }
}

object arbitrary extends arbitrary

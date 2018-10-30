package com.earnest.microtypical.instances.twitter

import cats.Monad
import com.twitter.util.Future

trait future {
  implicit val microtypicalInstancesTwitterFutureMonad: Monad [Future] = new Monad [Future] {
    def pure [A] (a: A): Future [A] = Future value a

    def flatMap [A, B] (fa: Future [A]) (f: A => Future [B]): Future [B] = fa flatMap f

    override def tailRecM [A, B] (a: A) (f: A => Future [Either [A, B]]): Future [B] =
      f (a) flatMap {
        case Left (next) => tailRecM (next) (f)
        case Right (done) => Future value done
      }
  }
}

object future extends future

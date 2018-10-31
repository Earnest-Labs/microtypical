package com.earnest.microtypical.syntax.scala

import scala.annotation.tailrec
import scala.language.implicitConversions

final class IterableOps [A] (val self: Iterable [A]) extends AnyVal {
  def foldWhile [B] (z: B) (f: (A, B) => Either [B, B]): B = {
    val it = self.iterator

    @tailrec
    def go (acc: Either [B, B]): B = acc match {
      case Left (done) => done
      case Right (continue) =>
        if (it.hasNext)
          go (f (it.next(), continue))
        else
          continue
    }

    go (Right (z))
  }

  def isDistinct: Boolean =
    foldWhile ((Set.empty [A], true)) {
      case (next, (s, _)) =>
        if (s contains next)
          Left ((s, false))
        else
          Right ((s + next, true))
    } ._2
}

trait ToIterableOps {
  implicit def toIterableOps [A] (self: Iterable [A]): IterableOps [A] =
    new IterableOps [A] (self)
}

object iterable extends ToIterableOps

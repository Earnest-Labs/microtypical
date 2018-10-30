package com.earnest.microtypical.syntax.cats

import cats.effect.IO
import scala.language.implicitConversions

final class IOOps [T] (val self: IO [T]) extends AnyVal {
  def runWithError(): Either [Throwable, T] = self.attempt.unsafeRunSync()
  def run(): T = self.unsafeRunSync()
}

trait ToIOps {
  implicit def toIOOps [T](self: IO [T]): IOOps [T] =
    new IOOps [T] (self)
}

object io extends ToIOps

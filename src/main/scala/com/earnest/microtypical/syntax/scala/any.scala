package com.earnest.microtypical.syntax.scala
import scala.language.implicitConversions

final class AnyOps [A] (val self: A) extends AnyVal {
  def withEffect (effect: A => Unit): A = { effect (self); self }
  def |> [B] (f: A => B): B = f (self)
}

trait ToAnyOps {
  implicit def toAnyOps [A] (self: A): AnyOps [A] =
    new AnyOps [A] (self)
}

object any extends ToAnyOps

package com.earnest.microtypical.syntax.scala

import com.earnest.microtypical.data.text.NonEmptyString
import scala.language.implicitConversions

final class StringOps (val self: String) extends AnyVal {
  def nesUnsafe: NonEmptyString = NonEmptyString.applyUnsafe(self)
}

trait ToStringOps {
  implicit def toStringOps (self: String): StringOps =
    new StringOps (self)
}

object string extends ToStringOps

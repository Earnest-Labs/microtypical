package com.earnest.microtypical.syntax.scala

import scala.language.implicitConversions

final class LongOps(val self: Long) extends AnyVal {
  def maybeInt: Option [Int] = Some (self.toInt) filter (_.toLong == self)
  def toIntUnsafe: Int = maybeInt .get
}

trait ToLongOps {
  implicit def toLongOps (self: Long): LongOps =
    new LongOps (self)
}

object long extends ToLongOps

package com.earnest.microtypical.data.time

import java.time.Instant
import java.time.temporal.ChronoUnit.MICROS

import com.earnest.microtypical.Show
import com.earnest.microtypical.data._
import com.earnest.microtypical.data.validation._

abstract sealed case class EpochMicrosecond (value: Long) extends Validated

object EpochMicrosecond extends BoundedCompanion.Inclusive [EpochMicrosecond, Long] {
  override protected [this] def wrap (r: Long): EpochMicrosecond = new EpochMicrosecond (r) {}
  override protected [this] def unwrap (v: EpochMicrosecond): Long = v.value
  override def ordering: Ordering [Long] = implicitly
  override def show: Show [Long] = implicitly

  override def min: Long = 0
  override def max: Long = Long.MaxValue

  object ofInstant extends ValidatedCompanion [EpochMicrosecond, Instant, Errors] {
    private val microsToNanos = 1000
    private val secondsToMicros = 1000000

    val bounds: Bounds [Instant] = Bounds inclusive (Instant.EPOCH, Instant.EPOCH plus (EpochMicrosecond.max, MICROS))

    val constraints: Constraints [Instant] =
      bounded (bounds) and
      constraint (_.getNano % microsToNanos == 0, "be divisible by microseconds")

    override val model: Model [EpochMicrosecond, Instant, Errors] =
      Model .instance [EpochMicrosecond, Instant, Errors] (
        validate [EpochMicrosecond, Instant] (
          constraints,
          i => EpochMicrosecond applyUnsafe (secondsToMicros * i.getEpochSecond + i.getNano / microsToNanos)),
        v => Instant ofEpochSecond (v.value / secondsToMicros, microsToNanos * (v.value % secondsToMicros)))

    def truncateToMicros (i: Instant): Instant =
      Instant ofEpochSecond (i.getEpochSecond, i.getNano / microsToNanos * microsToNanos)
  }
}

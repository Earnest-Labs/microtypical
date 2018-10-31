package com.earnest.microtypical.data

package object money {
  val centsToDollars: BigDecimal = BigDecimal (1, 2)
  val dollarsToCents: BigDecimal = 1 / centsToDollars

  val maxCents: Long = 99999999999L
  val minCents: Long = -maxCents

  val maxDollars: BigDecimal = centsToDollars * maxCents
  val minDollars: BigDecimal = centsToDollars * minCents
}

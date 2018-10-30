package com.earnest.microtypical.data

import com.earnest.microtypical.data.numeric.Gteq0
import com.earnest.microtypical.data.validation.Errors
import shapeless.ops.nat.ToInt
import shapeless.{Nat, Succ}

package object decimal {
  implicit class CountAliasOps [N <: Succ [_]] (self: ValidatedCompanion [Count [N], Int, _]) (implicit n: ToInt [N]) {
    def toGteq0 (value: Count [N]): Gteq0 [Int] = Count toGteq0 value
  }

  implicit class DecimalAliasOps [N <: Succ [_]] (self: ValidatedCompanion [Decimal [N], BigDecimal, _]) (implicit n: ToInt [N]) {
    def roundBigDecimal (value: BigDecimal): Decimal [N] = Decimal roundBigDecimal value
    def roundDouble (value: Double): Decimal [N] = Decimal roundDouble value
  }

  type Count1 = Count [Nat._1]
  val Count1: ValidatedCompanion [Count1, Int, Errors] = ValidatedCompanion from Count.model

  type Count2 = Count [Nat._2]
  val Count2: ValidatedCompanion [Count2, Int, Errors] = ValidatedCompanion from Count.model

  type Count3 = Count [Nat._3]
  val Count3: ValidatedCompanion [Count3, Int, Errors] = ValidatedCompanion from Count.model

  type Count4 = Count [Nat._4]
  val Count4: ValidatedCompanion [Count4, Int, Errors] = ValidatedCompanion from Count.model

  type Count5 = Count [Nat._5]
  val Count5: ValidatedCompanion [Count5, Int, Errors] = ValidatedCompanion from Count.model

  type Count6 = Count [Nat._6]
  val Count6: ValidatedCompanion [Count6, Int, Errors] = ValidatedCompanion from Count.model

  type Count7 = Count [Nat._7]
  val Count7: ValidatedCompanion [Count7, Int, Errors] = ValidatedCompanion from Count.model

  type Count8 = Count [Nat._8]
  val Count8: ValidatedCompanion [Count8, Int, Errors] = ValidatedCompanion from Count.model

  type Count9 = Count [Nat._9]
  val Count9: ValidatedCompanion [Count9, Int, Errors] = ValidatedCompanion from Count.model

  type Decimal1 = Decimal [Nat._1]
  val Decimal1: ValidatedCompanion [Decimal1, BigDecimal, Errors] = ValidatedCompanion from Decimal.model

  type Decimal2 = Decimal [Nat._2]
  val Decimal2: ValidatedCompanion [Decimal2, BigDecimal, Errors] = ValidatedCompanion from Decimal.model

  type Decimal3 = Decimal [Nat._3]
  val Decimal3: ValidatedCompanion [Decimal3, BigDecimal, Errors] = ValidatedCompanion from Decimal.model

  type Decimal4 = Decimal [Nat._4]
  val Decimal4: ValidatedCompanion [Decimal4, BigDecimal, Errors] = ValidatedCompanion from Decimal.model

  type Decimal5 = Decimal [Nat._5]
  val Decimal5: ValidatedCompanion [Decimal5, BigDecimal, Errors] = ValidatedCompanion from Decimal.model

  type Decimal6 = Decimal [Nat._6]
  val Decimal6: ValidatedCompanion [Decimal6, BigDecimal, Errors] = ValidatedCompanion from Decimal.model

  type Decimal7 = Decimal [Nat._7]
  val Decimal7: ValidatedCompanion [Decimal7, BigDecimal, Errors] = ValidatedCompanion from Decimal.model

  type Decimal8 = Decimal [Nat._8]
  val Decimal8: ValidatedCompanion [Decimal8, BigDecimal, Errors] = ValidatedCompanion from Decimal.model

  type Decimal9 = Decimal [Nat._9]
  val Decimal9: ValidatedCompanion [Decimal9, BigDecimal, Errors] = ValidatedCompanion from Decimal.model
}

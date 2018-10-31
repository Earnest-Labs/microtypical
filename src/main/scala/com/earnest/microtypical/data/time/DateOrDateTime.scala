package com.earnest.microtypical.data.time

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

import com.earnest.microtypical.data.BooleanExpression.{Or, Pure}
import com.earnest.microtypical.data.validation.Errors
import com.earnest.microtypical.data.{Model, Validated}
import org.scalacheck.{Arbitrary, Gen}

import scala.util.{Failure, Success, Try}

case class DateOrDateTime (date: EpochDay, time: Option [SecondOfDay]) extends Validated

object DateOrDateTime {
  implicit def arbitrary (
    implicit
    EpochDay: Arbitrary [EpochDay],
    SecondOfDay: Arbitrary [SecondOfDay],
  ): Arbitrary [DateOrDateTime] =
    Arbitrary (
      for {
        d <- EpochDay.arbitrary
        s <- Gen option SecondOfDay.arbitrary
      } yield
        apply (d, s))

  implicit val ofIso8601: Model [DateOrDateTime, String, Errors] =
    Model .instance (
      r =>
        Try (LocalDate parse (r, DateTimeFormatter.ISO_LOCAL_DATE)) match {
          case Success (d) =>
            EpochDay ofLocalDate d map (apply (_, None))
          case Failure (_) =>
            Try (LocalDateTime parse (r, DateTimeFormatter.ISO_LOCAL_DATE_TIME)) match {
              case Success (dt) =>
                for {
                  d <- EpochDay ofLocalDate dt.toLocalDate
                  t <- SecondOfDay ofLocalTime dt.toLocalTime
                } yield apply (d, Some (t))
              case Failure (_) =>
                Left (
                  Or (
                    Pure ("be a well formed local date"),
                    Pure ("be a well formed local date time")))
            }
        },
      v => {
        val date = EpochDay.ofLocalDate review v.date
        v.time match {
          case Some (time) =>
            LocalDateTime of (date, SecondOfDay.ofLocalTime review time) format DateTimeFormatter.ISO_LOCAL_DATE_TIME
          case None =>
            date format DateTimeFormatter.ISO_LOCAL_DATE
        }
      })
}

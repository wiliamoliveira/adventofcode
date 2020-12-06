package com.adventofcode.y2020.d04

import scala.io.Source

object SolutionDay04 extends App {
  val FILENAME = "day-04-input.txt"
  val PASSPORT_FIELD_SEPARATOR = " "
  val KEY_VALUE_SEPARATOR = ":"

  val passportsWithRequiredFields =
    Source
      .fromResource(FILENAME)
      .getLines
      .map(_.trim)
      .mkString(PASSPORT_FIELD_SEPARATOR)
      .split(PASSPORT_FIELD_SEPARATOR + PASSPORT_FIELD_SEPARATOR)
      .map(PassportString.unapply(_))
      .filter(_.isDefined)

  val validPassports =
    passportsWithRequiredFields
      .filter(_.get.isValid())

  println(s"Part 1: required fields present = ${passportsWithRequiredFields.size}")
  println(s"Part 2: required fields present and valid = ${validPassports.size}")
}

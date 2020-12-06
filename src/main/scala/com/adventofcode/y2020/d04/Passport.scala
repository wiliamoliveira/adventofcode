package com.adventofcode.y2020.d04

import com.adventofcode.y2020.d04.Passport._

case class Passport(byr: Int,
                    cid: String,
                    ecl: String,
                    eyr: Int,
                    hgt: Int,
                    hgtScale: String,
                    hcl: String,
                    iyr: Int,
                    pid: String) {

  private def isValidByr(): Boolean = {
    byr >= LOWEST_BYR && byr <= HIGHEST_BYR
  }

  private def isValidIyr(): Boolean = {
    iyr >= LOWEST_IYR && iyr <= HIGHEST_IYR
  }

  private def isValidEyr(): Boolean = {
    eyr >= LOWEST_EYR && eyr <= HIGHEST_EYR
  }

  private def isValidHgt(): Boolean = {
    if (hgtScale == CM) {
      hgt >= LOWEST_HGT_CM && hgt <= HIGHEST_HGT_CM
    } else if (hgtScale == IN) {
      hgt >= LOWEST_HGT_IN && hgt <= HIGHEST_HGT_IN
    } else {
      false
    }
  }

  private def isValidHcl(): Boolean = {
    HCL_REGEXP.matches(hcl)
  }

  private def isValidEcl(): Boolean = {
    validEcls.contains(ecl)
  }

  private def isValidPid(): Boolean = {
    PID_REGEXP.matches(pid)
  }

  def isValid(): Boolean = {
    isValidByr() &&
    isValidIyr() &&
    isValidEyr() &&
    isValidHgt() &&
    isValidHcl() &&
    isValidEcl() &&
    isValidPid()
  }
}

object Passport {
  val CM = "cm"
  val IN = "in"

  val LOWEST_BYR = 1920
  val HIGHEST_BYR = 2002

  val LOWEST_IYR = 2010
  val HIGHEST_IYR = 2020

  val LOWEST_EYR = 2020
  val HIGHEST_EYR = 2030

  val LOWEST_HGT_CM = 150
  val HIGHEST_HGT_CM = 193
  val LOWEST_HGT_IN = 59
  val HIGHEST_HGT_IN = 76

  val HCL_REGEXP = "^#[\\da-f]{6}$".r
  val PID_REGEXP = "^[\\d]{9}$".r

  val validEcls = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  val PASSPORT_FIELD_SEPARATOR = " "
  val KEY_VALUE_SEPARATOR = ":"
  val requiredFields = Seq("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
}

object PassportString {

  def unapply(str: String): Option[Passport] = {
    unapply(
      str
        .split(PASSPORT_FIELD_SEPARATOR)
        .flatMap(
          values =>
            values
              .split(KEY_VALUE_SEPARATOR)
              .grouped(2)
              .map { case Array(key, value) => key -> value }
        )
        .toMap
    )
  }

  def unapply(passportsMap: Map[String, String]): Option[Passport] = {
    if (requiredFields.forall(passportsMap.keySet.contains(_))) {
      val hgt = passportsMap.getOrElse("hgt", "0cm")

      Option(
        Passport(
          byr = passportsMap.getOrElse("byr", "0").toInt,
          cid = passportsMap.getOrElse("cid", ""),
          ecl = passportsMap.getOrElse("ecl", ""),
          eyr = passportsMap.getOrElse("eyr", "0").toInt,
          hgt = hgt.substring(0, hgt.size - 2).toInt,
          hgtScale = hgt.substring(hgt.size - 2, hgt.size),
          hcl = passportsMap.getOrElse("hcl", ""),
          iyr = passportsMap.getOrElse("iyr", "0").toInt,
          pid = passportsMap.getOrElse("pid", "")
        )
      )
    } else {
      None
    }
  }
}

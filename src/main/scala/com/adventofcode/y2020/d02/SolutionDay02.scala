package com.adventofcode.y2020.d02

import scala.io.Source

object SolutionDay02 extends App {
  val FILENAME = "day-02-input.txt"

  def solve(checkPassword: Password => Boolean, passwords: List[Password]): Unit = {
    val validPassword =
      passwords
        .filter(checkPassword(_))

    validPassword.foreach(println(_))
    println(s"Number of valid passwords: ${validPassword.size}")
  }

  def checkPasswordPart1(p: Password): Boolean = {
    val letterCount = p.password.count(_ == p.letter)
    (letterCount >= p.rule1) && (letterCount <= p.rule2)
  }

  def checkPasswordPart2(p: Password): Boolean = {
    val char1 = p.password.charAt(p.rule1 - 1)
    val char2 = p.password.charAt(p.rule2 - 1)

    (char1 == p.letter || char2 == p.letter) && (char1 != p.letter || char2 != p.letter)
  }

  val passwords =
    Source
      .fromResource(FILENAME)
      .getLines
      .toList
      .flatMap(PasswordString.unapply(_))

  solve(checkPasswordPart1, passwords)
  solve(checkPasswordPart2, passwords)

}

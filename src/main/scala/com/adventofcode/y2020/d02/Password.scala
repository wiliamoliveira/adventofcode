package com.adventofcode.y2020.d02

case class Password(rule1: Int, rule2: Int, letter: Char, password: String)

object PasswordString {
  val PasswordRegex = "^(\\d+)-(\\d+)\\s([a-z]):\\s([a-z]+)$".r

  def unapply(str: String): Option[Password] = str match {
    case PasswordRegex(min, max, letter, password) =>
      Option(Password(min.toInt, max.toInt, letter.toCharArray.head, password))
    case _ => None
  }
}

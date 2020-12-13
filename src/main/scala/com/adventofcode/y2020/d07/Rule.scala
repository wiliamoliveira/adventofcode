package com.adventofcode.y2020.d07

case class Rule(bag: Bag, contain: Map[Bag, Int])

object Rule {
  val COMMA = ","

  val RuleRegexp = "^([a-z\\s]+)(?:\\s+)(?:bag|bags){1}(?:\\s+)(?:contain){1}(?:\\s+)(.*)(?:\\.?)$".r
  val SingleRuleRegexp = "^(\\d+)(?:\\s+)([a-z\\s]+)(?:bag|bags){1}(?:\\.?)$".r

  def apply(str: String): Option[Rule] = str match {
    case RuleRegexp(bag, contain) =>
      Option(
        Rule(
          Bag(bag.trim),
          transformContainInBags(contain)
        )
      )
    case _ => None
  }

  def transformContainInBags(contain: String): Map[Bag, Int] = {
    contain
      .split(COMMA)
      .toList
      .map(_.trim)
      .map {
        case SingleRuleRegexp(count, bag) => Option(Bag(bag.trim) -> count.toInt)
        case _ => None
      }
      .flatten
      .toMap
  }

}

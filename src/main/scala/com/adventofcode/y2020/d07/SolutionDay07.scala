package com.adventofcode.y2020.d07

import scala.io.Source

object SolutionDay07 extends App {
  val FILENAME = "day-07-input.txt"
  val MY_BAG = Bag("shiny gold")

  def countBagsPart1(bags: Set[Bag], rules: Map[Bag, Set[Bag]]): Set[Bag] = {
    if (bags.size <= 0) {
      Set()
    } else {
      val contain =
        rules
          .filter(
            rule => bags.exists(rule._2.contains)
          )
          .map(_._1)
          .toSet

      bags ++ countBagsPart1(contain, rules)
    }
  }

  def solvePart1(rules: List[Rule]): Unit = {
    val rulesMap =
      rules
        .map(r => r.bag -> r.contain.keySet)
        .toMap

    val result = countBagsPart1(Set(MY_BAG), rulesMap).filterNot(_ == MY_BAG).size

    println(s"Part 1 - number of bags colors contain at least one '${MY_BAG.name}' bag is $result")
  }

  val rules =
    Source
      .fromResource(FILENAME)
      .getLines
      .flatMap(Rule.apply(_))
      .toList

  solvePart1(rules)

}

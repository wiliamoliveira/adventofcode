package com.adventofcode.y2020.d01

import scala.io.Source

object SolutionDay01 extends App {
  val FILENAME = "day-01-input.txt"
  val SUM = 2020

  def product(combination: Int): Unit =
    Source
      .fromResource(FILENAME)
      .getLines
      .toList
      .map(_.toInt)
      .filterNot(_ > SUM)
      .combinations(combination)
      .filter(_.sum == SUM)
      .foreach(item => println(s"for combinations of $combination elements: $item = ${item.product}"))

  product(2)
  product(3)
}

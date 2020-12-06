package com.adventofcode.y2020.d03

import scala.io.Source

object SolutionDay03 extends App {
  val FILENAME = "day-03-input.txt"
  val TREE = '#'

  def isTree(line: String, lineNumber: Int, right: Int, down: Int): Boolean = {
    if (lineNumber % down == 0) {
      val pos = (lineNumber / down * right) % line.size
      line.charAt(pos) == TREE
    } else {
      false
    }
  }

  def numberOfTrees(linesWithIndex: List[(String, Int)], right: Int, down: Int): Int = {
    val trees =
      linesWithIndex.filter {
        case (line, lineNumber) => isTree(line, lineNumber, right, down)
      }.size

    println(s"numberOfTrees with slope right $right, down $down = $slopes1to1 trees")

    trees
  }

  val lines =
    Source
      .fromResource(FILENAME)
      .getLines
      .toList
      .zipWithIndex

  val slopes1to1 = numberOfTrees(lines, 1, 1)
  val slopes3to1 = numberOfTrees(lines, 3, 1)
  val slopes5to1 = numberOfTrees(lines, 5, 1)
  val slopes7to1 = numberOfTrees(lines, 7, 1)
  val slopes1to2 = numberOfTrees(lines, 1, 2)

  val part1 = slopes3to1
  val part2 = slopes1to1.toLong * slopes3to1.toLong * slopes5to1.toLong * slopes7to1.toLong * slopes1to2.toLong

  println(s"Part 1: $part1 trees")
  println(s"Part 2: $part2 trees")
}

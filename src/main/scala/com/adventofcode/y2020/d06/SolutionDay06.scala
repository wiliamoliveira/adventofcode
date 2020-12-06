package com.adventofcode.y2020.d06

import scala.io.Source

object SolutionDay06 extends App {
  val FILENAME = "day-06-input.txt"
  val TMP_SEPARATOR = " "
  val SPACE = ' '

  def anyoneAnsweredYes(answers: String): Int = {
    answers
      .toCharArray
      .filterNot(_ == SPACE)
      .toSet
      .size
  }

  def everyoneAnswerYes(answers: String): Int = {
    val groupSize = answers.count(_ == SPACE) + 1

    answers
      .toCharArray
      .filterNot(_ == SPACE)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .filter(_._2 == groupSize)
      .size
  }

  def solve(answer: String => Int, groupAnswers: Array[String]): Int = {
    groupAnswers
      .map(answer(_))
      .sum
  }

  val groupAnswers =
    Source
      .fromResource(FILENAME)
      .getLines
      .map(_.trim)
      .mkString(TMP_SEPARATOR)
      .split(TMP_SEPARATOR + TMP_SEPARATOR)

  val part1Sum = solve(anyoneAnsweredYes, groupAnswers)
  val part2Sum = solve(everyoneAnswerYes, groupAnswers)

  println(s"Part 1 - sum of anyone answered yes is $part1Sum")
  println(s"Part 2 - sum of everyone answered yes is $part2Sum")
}

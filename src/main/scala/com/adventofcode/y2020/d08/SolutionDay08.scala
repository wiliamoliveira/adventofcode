package com.adventofcode.y2020.d08

import scala.io.Source
import scala.util.Try

object SolutionDay08 extends App {
  val FILENAME = "day-08-input.txt"
  val ACC_REGEXP = "^acc\\s([-+]{1}\\d+)$".r
  val JMP_REGEXP = "^jmp\\s([-+]{1}\\d+)$".r
  val NOP_REGEXP = "^nop\\s([-+]{1}\\d+)$".r

  def calcAccumulator(program: List[String]): (Int, Boolean) = {
    var accumulator = 0
    var nextCommand = 0
    var outOfBounds = false
    var commandsExecuted = scala.collection.mutable.SortedSet[Int]()

    while (!commandsExecuted.contains(nextCommand) && nextCommand < program.size) {
      commandsExecuted += nextCommand
      Try(program(nextCommand)).toOption match {
        case Some(ACC_REGEXP(value)) => {
          accumulator += value.toInt
          nextCommand += 1
        }
        case Some(JMP_REGEXP(value)) => {
          nextCommand += value.toInt
        }
        case None =>
          outOfBounds = true
        case _ => {
          nextCommand += 1
        }
      }
    }

    (accumulator, !commandsExecuted.contains(nextCommand) && !outOfBounds)
  }

  def calcAccumulatorFirstValidProgram(program: List[String]): Int = {
    program
      .zipWithIndex
      .map {
        case (JMP_REGEXP(value), index) => Option(calcAccumulator(program.patch(index, Seq(s"nop $value"), 1)))
        case (NOP_REGEXP(value), index) => Option(calcAccumulator(program.patch(index, Seq(s"jmp $value"), 1)))
        case _ => None
      }
      .flatMap {
        case Some((accumulator, true)) => Option(accumulator.toInt)
        case _ => None
      }
      .head
  }

  def solvePart1(program: List[String]): Unit = {
    val (accumulator, _) = calcAccumulator(program)

    println(s"Part 1 - accumulator before infinite loop is $accumulator")
  }

  def solvePart2(program: List[String]): Unit = {
    val accumulator = calcAccumulatorFirstValidProgram(program)
    println(s"Part 2 - accumulator after the program terminates is $accumulator")
  }

  val lines =
    Source
      .fromResource(FILENAME)
      .getLines
      .toList

  solvePart1(lines)
  solvePart2(lines)

}

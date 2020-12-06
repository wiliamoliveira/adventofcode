package com.adventofcode.y2020.d05

import scala.annotation.tailrec
import scala.io.Source

object SolutionDay05 extends App {
  val FILENAME = "day-05-input.txt"

  val BOARDING_PASS_ROW_LEN = 7

  val ROW_LOW_CHAR = 'F'
  val ROW_MIN = 0
  val ROW_MAX = 127

  val COLUMN_LOW_CHAR = 'L'
  val COLUMN_MIN = 0
  val COLUMN_MAX = 7

  @tailrec
  def calc(boardingPass: String, index: Int, min: Int, max: Int, lowChar: Char): Int = {
    if (index == boardingPass.size - 1) {
      if (boardingPass.charAt(boardingPass.size - 1) == lowChar) {
        min
      } else {
        max
      }
    } else {
      if (boardingPass.charAt(index) == lowChar) {
        val newMax = math.floor((min + max) / 2).toInt
        calc(boardingPass, index + 1, min, newMax, lowChar)
      } else {
        val newMin = math.floor(((min + max) / 2) + 1).toInt
        calc(boardingPass, index + 1, newMin, max, lowChar)
      }
    }
  }

  def calcRow(line: String): Int = {
    val rowBoardingPass = line.substring(0, BOARDING_PASS_ROW_LEN)
    calc(rowBoardingPass, 0, ROW_MIN, ROW_MAX, ROW_LOW_CHAR)
  }

  def calcColumn(line: String): Int = {
    val columnBoardingPass = line.substring(BOARDING_PASS_ROW_LEN, line.size)
    calc(columnBoardingPass, 0, COLUMN_MIN, COLUMN_MAX, COLUMN_LOW_CHAR)
  }

  def calcId(line: String): Int = {
    val row = calcRow(line)
    val column = calcColumn(line)
    (row * 8) + column
  }

  def missingId(boardingList: List[Int]): Int = {
    val sortedList = boardingList.sorted

    val min = sortedList.sorted.head
    val max = sortedList.sorted.last

    (min to max).toSet.diff(sortedList.toSet).head

  }

  val boardingList =
    Source
      .fromResource(FILENAME)
      .getLines
      .map(calcId(_))
      .toList

  val maxId = boardingList.max
  val myId = missingId(boardingList)

  println(s"Part 1 - max ID is $maxId")
  println(s"Part 2 - my ID is $myId")
}

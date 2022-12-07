package day6

import helpers.Helpers

object Day6 {
  def main(args: Array[String]): Unit = {
    val rawLines = Helpers.readFile("day6/day6.txt")
    val part1 = rawLines.map(findIndex(_,4))
    println(s"Part 1: $part1")

    val part2 = rawLines.map(findIndex(_,14))
    println(s"Part 2: $part2")
  }

  def findIndex(line: String, windowSize: Int) = {
    line.toCharArray.sliding(windowSize, 1)
      .zipWithIndex
      .find { case (entry, index) => entry.toSet.size == windowSize }
      .map(x => (x._1.mkString(""), x._2 + windowSize))
      .head
      ._2 + windowSize
  }
}

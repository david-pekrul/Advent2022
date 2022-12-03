package day3

import helpers.Helpers

import java.io.File
import scala.io.Source

object Day3 {
  def main(args: Array[String]): Unit = {
    val part1 = Helpers.readFile("day3/day3.txt").map(line => {
      Rucksack(line).getValue()
    }).map(_._2).sum
    println(s"Part 1: ${part1}")

    val part2 = Helpers.readFile("day3/day3.txt").grouped(3).map(group => {
      ElfGroup(group).getValue()
    }).map(_._2).sum

    println(s"Part 2 : $part2")

  }
}

case class Rucksack(input: String) {
  def getSharedLetter(): Char = {
    def first = input.substring(0, input.length / 2)
    def second = input.substring(input.length / 2)
    first.toCharArray.toSet.intersect(second.toCharArray.toSet).head
  }

  def getValue() = {
    val sharedLetter = getSharedLetter()
    (sharedLetter,Score.getScore(sharedLetter))
  }
}

case class ElfGroup(val lines: Seq[String]) {
  def getSharedLetter(): Char = {
    val allChars = lines.map(_.toSet).fold(Set())((acc,next) => acc ++ next)
    lines.map(_.toSet).fold(allChars)((acc,next) => acc.intersect(next)).head
  }

  def getValue() = {
    val sharedLetter = getSharedLetter()
    (sharedLetter, Score.getScore(sharedLetter))
  }
}

object Score {
  def getScore(input: Char) = {
    val intValue = input.asInstanceOf[Int]
    if (input.isLower) {
      intValue - 96
    } else {
      intValue - 38
    }
  }

}

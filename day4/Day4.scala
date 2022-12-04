package day4

import helpers.Helpers

object Day4 {
  def main(args: Array[String]): Unit = {
    val part1 = Helpers.readFile("day4/day4.txt")
      .map(Range.parseRange)
      .map(x => (x, x.isOneSideFullyCovered()))
      .filter(_._2)
      .size
    println(s"Part 1: $part1")

    val part2 = Helpers.readFile("day4/day4.txt")
      .map(Range.parseRange)
      .map(x => (x, x.doRangesOverlap()))
      .filter(_._2)
      .size
    println(s"Part 2: $part2")

  }
}

case class Range(a: InnerRange, b: InnerRange) {
  def isOneSideFullyCovered(): Boolean = {
    val narrow = if (a.width < b.width) a else b
    val wide = if (narrow == b) a else b

    if (narrow.low < wide.low || narrow.high > wide.high) {
      //one of the narrower ranges ends is not covered by the wider range
      return false
    }
    true
  }

  def doRangesOverlap(): Boolean = {
    val narrow = if (a.width < b.width) a else b
    val wide = if (narrow == b) a else b

    if (narrow.high < wide.low || narrow.low > wide.high) {
      return false
    }

    true
  }
}

case class InnerRange(low: Int, high: Int) {
  val width = high - low;
}

object Range {

  val regexPattern = "(\\d+)-(\\d+),(\\d+)-(\\d+)".r
  def parseRange(input: String) = {
    val regexPattern(a, b, c, d) = input
    Range(InnerRange(f(a), f(b)), InnerRange(f(c), f(d)))
  }
  def f(x: String) = Integer.parseInt(x)
}

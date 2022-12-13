package day10

import helpers.Helpers

object Day10 {
  def main(args: Array[String]): Unit = {
    val instructions = Helpers.readFile("day10/day10.txt")
    val addPattern = "addx (-?\\d+)".r
    val checkpoints = Seq(20, 60, 100, 140, 180, 220)

    val part1Processed = instructions.foldLeft((Seq(State(1, 1))))((acc, next) => {
      val nextState = if (next.equals("noop")) {
        Seq(acc.last.next(1, 0))
      } else {
        val addPattern(dX) = next
        Seq(acc.last.next(1, 0), acc.last.next(2, Integer.parseInt(dX)))
      }
      acc ++ nextState
    })
      .map(s => s.cycle -> s.getScore()).toMap

    val part1 = checkpoints.map(c => part1Processed.get(c).get).sum
    println(part1)


  }
}

case class State(cycle: Int, x: Int) {
  def next(c: Int, dX: Int) = {
    State(cycle + c, x + dX)
  }

  def getScore(): Int = {
    cycle * x
  }
}

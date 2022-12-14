package day10

import helpers.Helpers

object Day10 {
  def main(args: Array[String]): Unit = {
    val instructions = Helpers.readFile("day10/day10.txt")
    val addPattern = "addx (-?\\d+)".r
    val checkpoints = Seq(20, 60, 100, 140, 180, 220)

    val cycleAndXs = instructions.foldLeft((Seq(State(1, 1))))((acc, next) => {
      val nextState = if (next.equals("noop")) {
        Seq(acc.last.next(1, 0))
      } else {
        val addPattern(dX) = next
        Seq(acc.last.next(1, 0), acc.last.next(2, Integer.parseInt(dX)))
      }
      acc ++ nextState
    })

    val part1Scores = cycleAndXs.map(s => s.cycle -> s.getScore()).toMap

    val part1 = checkpoints.map(c => part1Scores.get(c).get).sum
    println(part1)

    println("Part 2: ")
    cycleAndXs.foreach(state => {
      if(state.cycle%40 == 1){
        println
      }
      val pixelRange = (state.x - 1 to state.x + 1)
      if(pixelRange.contains((state.cycle-1)%40)){
        print("#")
      } else {
        print(" ")
      }

    })
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

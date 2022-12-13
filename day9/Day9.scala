package day9

import helpers.Helpers

object Day9 {
  def main(args: Array[String]): Unit = {
    def rawLines = Helpers.readFile("day9/day9.txt")

    val instructions = rawLines.map(Instruction.parse)
    //    printVisited(visited)

    val part1 = Runner.process2(1,instructions).size
    println(s"Part 1: $part1") //5710

    val part2 = Runner.process2(9,instructions).size
    println(s"Part 2: ${part2}") //2259
  }

  def printVisited(visitedCoords: Set[(Int, Int)]): Unit = {
    val rowRange = visitedCoords.map(_._1).toSeq.sorted
    val (minRow, maxRow) = (rowRange.head, rowRange.last)
    val colRange = visitedCoords.map(_._2).toSeq.sorted
    val (minCol, maxCol) = (colRange.head, colRange.last)

    (maxRow to minRow by -1).foreach(row => {
      (minCol to maxCol).foreach(col => {
        if (visitedCoords.contains((row, col))) {
          print("#")
        } else {
          print(".")
        }
      })
      println
    })
  }
}

object Runner {
  def process2(ropeLength: Int, instructions: Seq[Instruction]): Set[(Int, Int)] = {

    val knots: Seq[Coord] = (0 to ropeLength).map(_ => Coord(0,0))
    val tailVisitedCoords: Set[(Int, Int)] = Set((knots.last.row,knots.last.col))
    val allInstructions = instructions.map(_.getSeq()).flatten

    allInstructions.foldLeft((knots,tailVisitedCoords))((prevInfo,nextInstruction) => {
      val prevKnots = prevInfo._1
      val prevVisitedCoords = prevInfo._2
      val headMove = prevKnots.head.add(nextInstruction.direction)
      val updatedKnots = prevKnots.foldLeft(Seq[Coord]())((acc,next) => {
        if(acc.isEmpty){
          Seq(next.add(nextInstruction.direction))//move the lead knot
        } else {
          //follow the last knot in the accumulator
          acc :+ next.trail(acc.last)._1
        }
      })
      val updatedVisited = prevInfo._2 + updatedKnots.last.getTuple()
      (updatedKnots,updatedVisited)
    })._2
  }
}
case class Coord(row: Int, col: Int) {
  def add(step: Coord): Coord = {
    Coord(row + step.row, col + step.col)
  }

  def getTuple(): (Int,Int) = {
    (row,col)
  }

  def trail(head: Coord): (Coord, Boolean) = {
    val deltaR = head.row - row;
    val deltaC = head.col - col;

    /*  (rows,columns) relative to Tail position //not x,y

                 (2,-1)A       (2,0)B      (2,1)C

       (1,-2)D   (1,-1)A'D'    (1,0)B'     (1,1)C'E'   (1,2)E

       (0,-2)F   (0,-1)F'     *(0,0)T*     (0,1)G'     (0,2)G

       (-1,-2)H  (-1,-1)H'J'   (-1,0)K'    (-1,1)I'L'  (-1,2)I

                 (-2,-1)J      (-2,0)K      (-2,1)L


    */
    val noOps = (-1 to 1).map(r => {
      (-1 to 1).map(c => {
        (r, c)
      })
    }).flatten.toSet

    val move: Coord = (deltaR, deltaC) match {
      case (2, 1) | (1, 2) => Coord(1, 1) //C,E => C'E'
      case (0, 2) => Coord(0, 1) //G   => G'
      case (-1, 2) | (-2, 1) => Coord(-1, 1) //I,L => I'L'
      case (-2, 0) => Coord(-1, 0) //K   => K'
      case (-2, -1) | (-1, -2) => Coord(-1, -1) //H,J => H'J'
      case (0, -2) => Coord(0, -1) //F   => F'
      case (1, -2) | (2, -1) => Coord(1, -1) //D,A => D'A'
      case (2, 0) => Coord(1, 0) //B   => B'
      //part 2 cases:
      case (-2,-2) => Coord(-1,-1)
      case (2,-2) => Coord(1,-1)
      case (-2,2) => Coord(-1,1)
      case (2,2) => Coord(1,1)

      case _ => {
        if (noOps.contains((deltaR, deltaC))) {
          Coord(0, 0)
        } else {
          println(s"Can't handle ${(deltaR, deltaC)}")
          throw new RuntimeException("unexpected move!")
        }
      } //No move needed
    }
    (this.add(move), move.col == 0 && move.row == 0)
  }
}

object Instruction {
  val pattern = "(\\w)\\s(\\d+)".r

  def parse(input: String): Instruction = {
    val pattern(d, q) = input
    val coord = d match {
      case "U" => UP
      case "D" => DOWN
      case "L" => LEFT
      case "R" => RIGHT
    }
    Instruction(Integer.parseInt(q), coord)
  }

  val UP = Coord(1, 0)
  val DOWN = Coord(-1, 0)
  val LEFT = Coord(0, -1)
  val RIGHT = Coord(0, 1)
}

case class Instruction(quantity: Int, direction: Coord) {
  def getSeq(): Seq[SimpleInstruction] = {
    (1 to quantity).map(_ => SimpleInstruction(direction))
  }

  override def toString: String = {
    s"I[$quantity,$direction]"
  }
}

case class SimpleInstruction(direction: Coord)

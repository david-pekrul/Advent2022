package day14

import helpers.Helpers

import scala.annotation.tailrec

object Day14 {
  /** *
   * THOUGHT: Sand might not ever land on the lowest rocks. There might be a gap from a middle layer all the way to the abyss.
   *
   */


  def main(args: Array[String]): Unit = {
    val rockPaths = Helpers.readFile("day14/day14.txt").toSeq.map(RockPath.parse).flatten
    val caveStart = Cave.build(rockPaths)
//    caveStart.printCave()

    val part1 = Cave.fillCave(caveStart)
    println(s"Part 1: $part1")


  }
}

case class RockPath(start: Coord, end: Coord) {
  def getRockCoords(): Seq[Coord] = {
    //determine horizontal or vertical
    if (start.row == end.row) {
      val row = start.row
      val minCol = start.col min end.col
      val maxCol = start.col max end.col

      (minCol to maxCol).map(col => {
        Coord(row, col)
      })
    } else {
      val col = start.col
      val minRow = start.row min end.row
      val maxRow = start.row max end.row
      (minRow to maxRow).map(row => {
        Coord(row, col)
      })
    }
  }
}

case class Coord(row: Int, col: Int) {
  def down() = Coord(row + 1, col)

  def downLeft() = Coord(row + 1, col - 1)

  def downRight() = Coord(row + 1, col + 1)
}

object RockPath {

  val regex = """(\d+),(\d+)""".r

  def parse(rawInput: String): Seq[RockPath] = {

    def f(s: String) = Integer.parseInt(s)

    rawInput.split(" -> ").map(next => {
      val regex(c, r) = next.trim
      Coord(f(r), f(c))
    })
      .toSeq
      .sliding(2, 1)
      .map(pair => {
        RockPath(pair(0), pair(1))
      })
      .toSeq

  }
}

case class Cave(cave: Map[Coord, Option[Boolean]], abyssDepth: Option[Int] = None) {
  /*
      None => Empty
      Some(true) => Rock
      Some(false) => Sand
   */

  lazy val getAbyssDepth = {
    if (abyssDepth.isDefined) {
      abyssDepth.get
    } else {
      cave.keys.foldLeft(0)((acc, next) => {
        Math.max(acc, next.row)
      })
    }
  }

  val sandSource = Coord(0, 500)


  /**
   * @return Some(cave) if the sand came to a rest, None otherwise
   */
  def addSand(): Cave = {

    //tail recursive until the sand can't fall anymore

    def run(currentCave: Cave, sand: Sand): Cave = {
      //Updated Cave + Is Sand at rest?

      val newSandLocationOpt = sand.getNextFallCoord(currentCave)
      if (newSandLocationOpt.isEmpty) {
        //the sand is at rest
        return currentCave
      }

      val newSandLocation = newSandLocationOpt.get

      if (newSandLocation.row >= currentCave.getAbyssDepth) {
        return Cave.CAVE_AT_MAX_CAPACITY
      }

      val updatedCave = currentCave.cave
        .updated(sand.coord, None) //remove the sand from it's previous spot
        .updated(newSandLocation, Some(false)) //put it in the new spot
      return run(Cave(updatedCave, Some(getAbyssDepth)), Sand(newSandLocation))
    }

    run(this, Sand(sandSource))

  }

  def printCave(): Unit = {
    val rocks = cave.keys.toSeq
    val (minRow, maxRow, minCol, maxCol) = rocks.foldLeft((rocks.head.row, rocks.head.row, rocks.head.col, rocks.head.col))((acc, next) => {
      val rows = Seq(acc._1, acc._2, next.row, next.row)
      val cols = Seq(acc._3, acc._4, next.col, next.col)
      (
        rows.min,
        rows.max,
        cols.min,
        cols.max
      )
    })

    val extremes = (
      minRow,
      maxRow,
      minCol,
      maxCol
    )

    println(extremes)

    (0 to maxRow).foreach(row => {
      print(s"$row\t")
      (minCol to maxCol).foreach(col => {
        val current = Coord(row, col)
        if (current == sandSource) {
          print("+")
        } else {
          cave.get(current).getOrElse(None) match {
            case None => print(".")
            case Some(false) => print("o")
            case Some(true) => print("#")
          }
        }
      })
      println
    })
  }

}

case class Sand(coord: Coord) {
  def getNextFallCoord(cave: Cave): Option[Coord] = {
    val fallDirections = Seq(coord.down, coord.downLeft(), coord.downRight())
    val emptyLocationsInOrder = fallDirections
      .map(d => (d, cave.cave.get(d).getOrElse(None)))
      .filter(_._2.isEmpty)

    emptyLocationsInOrder.headOption match {
      case None => None //This can't fall
      case Some(x) => Some(x._1) //This is the next location it can fall
    }
  }
}

object Cave {

  def build(rocks: Seq[RockPath]): Cave = {

    val rockCoords: Seq[Coord] = rocks.flatMap(_.getRockCoords())

    //Place the rocks on in the cave
    val mappedCoords = rockCoords.foldLeft(Map[Coord, Option[Boolean]]())((acc, next) => {
      acc.updated(next, Some(true))
    })

    Cave(mappedCoords)
  }

  val CAVE_AT_MAX_CAPACITY = Cave(Map.empty)

  def fillCave(inputCave: Cave): Int = {
    @tailrec
    def addMoreSand(currentCave: Cave, count: Int): Int = {
      val nextCave = currentCave.addSand()
      if (nextCave == CAVE_AT_MAX_CAPACITY) {
        return count
      }
//      println(s"So far: $count")
//      nextCave.printCave()
//      println("---------------------")
      addMoreSand(nextCave, count + 1)
    }

    addMoreSand(inputCave, 0)
  }

}

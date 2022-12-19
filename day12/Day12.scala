package day12

import helpers.Helpers

import scala.annotation.tailrec

object Day12 {
  def main(args: Array[String]): Unit = {
    val rawInputLines = Helpers.readFile("day12/day12.txt")
    val letterHeights = rawInputLines.zipWithIndex.map { case (rawLine, rowIndex) =>
      rawLine.toCharArray.zipWithIndex.map { case (char, colIndex) => {
        LetterHeight(char, rowIndex, colIndex)
      }
      }.toSeq
    }.flatten

    val letterHeightMap = letterHeights.map(lh => (lh.row, lh.col) -> lh).toMap
    val S = getStartLocation(letterHeightMap)
    val E = getEndLocation(letterHeightMap)
    MapStats.setMax(letterHeightMap)

    //    println(letterHeightMap)

    val calculated = bfs(S, letterHeightMap)

    //    println(calculated)

    printMap(calculated)

    val part1 = calculated.get(E.getCoords()).get.depth
    println(s"Part 1: $part1")
    //338 too low
    //470 too high

    val minForLetters = alphabet.map(char => {
      calculated.values
        .filter(_.visited())
        .filter(spot => spot.char == char)
        .reduce((a,b) => if(a.depth < b.depth) {a} else {b})
        .printString
    }).toSeq
    println(minForLetters)


  }

  val alphabet = ('a' to 'z')

  def printMap(input: Map[(Int, Int), LetterHeight]): Unit = {
    (0 to MapStats.maxRow).foreach(row => {
      (0 to MapStats.maxCol).foreach(col => {
        print(input.get(row,col).get.printString + "\t")
      })
      println
    })
  }

  def getStartLocation(lm: Map[(Int, Int), LetterHeight]) = {
    lm.values.find(lh => lh.char == 'S').get
  }

  def getEndLocation(lm: Map[(Int, Int), LetterHeight]) = {
    lm.values.find(lh => lh.char == 'E').get
  }

  def bfs(start: LetterHeight, map: Map[(Int, Int), LetterHeight]) = {

    @tailrec
    def bfs2(currentMap: Map[(Int, Int), LetterHeight], nextLayer: Set[(Int, Int)], depth: Int = 0): Map[(Int, Int), LetterHeight] = {
      val updatedMap = nextLayer.foldLeft(currentMap)((updatedMap, coord) => {
        updatedMap.updated(coord, updatedMap.get(coord).get.visit(depth))
      })
      val x = nextLayer
        .map(n => {
          val here = updatedMap.get(n).get
          val allNeighbors = here.neighborCoords
          val validSteps = allNeighbors
            .filter(x => {
              val neighborHeight = updatedMap.get(x).get.height
              (neighborHeight <= (here.height + 1))
            })
          validSteps
        })
        .flatten
        .map(coord => updatedMap.get(coord).get)
        .filter(!_.visited())
      val y = x
        .map(_.getCoords())
      if (y.isEmpty) {
        return updatedMap
      }
      bfs2(updatedMap, y, depth + 1)
    }

    bfs2(map, Set(start.getCoords()))
  }
}

object MapStats {
  var maxRow: Int = 0
  var maxCol: Int = 0

  def setMax(lm: Map[(Int, Int), LetterHeight]): Unit = {
    val maxes = lm.keys.foldLeft((0, 0))((acc, next) => {
      (Math.max(acc._1, next._1), Math.max(acc._2, next._2))
    })
    maxRow = maxes._1
    maxCol = maxes._2
  }
}

case class LetterHeight(char: Char, row: Int, col: Int, depth: Int = -1) {
  lazy val height = {
    char match {
      case 'E' => 27
      case 'S' => 0
      case _ => Helpers.getLetterIndex(char)
    }
  }

  def getCoords() = (row, col)

  lazy val neighborCoords: Seq[(Int, Int)] = {

    Seq(
      (row - 1, col),
      (row + 1, col),
      (row, col - 1),
      (row, col + 1)
    )
      .filter { case (nextRow, nextCol) => {
        !(nextRow < 0 || nextRow > MapStats.maxRow || nextCol < 0 || nextCol > MapStats.maxCol)
      }
      }
  }

  def visited() = depth != -1

  def visit(d: Int): LetterHeight = {
    LetterHeight(char, row, col, depth = d)
  }

  def printString = {
    s"[$char,$depth]"
  }
}


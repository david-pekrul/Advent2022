package day12

import helpers.Helpers

object Day12 {
  def main(args: Array[String]): Unit = {
    val rawInputLines = Helpers.readFile("day12/test.txt")
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


    println(letterHeightMap)
  }

  def getStartLocation(lm: Map[(Int, Int), LetterHeight]) = {
    lm.values.find(lh => lh.char == 'S').get
  }

  def getEndLocation(lm: Map[(Int, Int), LetterHeight]) = {
    lm.values.find(lh => lh.char == 'E').get
  }

  def flood(currentLocation: LetterHeight, map: Map[(Int, Int), LetterHeight]): Map[(Int, Int), LetterHeight] = {
    val pathSoFar = currentLocation.path
    val neighbors = currentLocation.getNeighborCoords().map(c => map.get(c).get)

  }

  //  def getPaths(start: LetterHeight, end: LetterHeight, map: Map[(Int,Int),LetterHeight]): Seq[Seq[LetterHeight]] = {
  //
  //    def depthFirstSearch(current: LetterHeight, end: LetterHeight, visited: Seq[LetterHeight])
  //
  //  }

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

case class LetterHeight(char: Char, row: Int, col: Int, path: Seq[LetterHeight] = Seq(), visited: Boolean = false) {
  lazy val height = Helpers.getLetterIndex(char)

  def getCoords() = (row, col)

  def getNeighborCoords(): Seq[(Int, Int)] = {
    (row - 1 to row + 1).map(nextRow => {
      (col - 1 to col + 1).map(nextCol => {
        if (nextRow < 0 || nextRow > MapStats.maxRow || nextCol < 0 || nextCol > MapStats.maxCol) {
          None
        } else if (nextRow == row && nextCol == col) {
          None
        } else {
          Some((nextRow, nextCol))
        }
      })
    }).flatten.filter(_.isDefined).map(_.get)
  }

  def addPath(incoming: Seq[LetterHeight]): LetterHeight = {
    LetterHeight(
      char,
      row,
      col,
      path = incoming,
      visited = true
    )
  }
}


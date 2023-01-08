package day22

import helpers.Helpers

object Day22 {
  def main(args: Array[String]): Unit = {
    val rawLines = Helpers.readFile("day22/day22.txt")

    val builtMap = parseMap(rawLines.init.init)
    val instructions = parseInstructions(rawLines.last)

    val initialStateCol = builtMap.rowBoundsMap(0).firstCol
    val initialState = WalkState(Coord(0, initialStateCol), Direction('E'))

    val finalState = instructions.foldLeft(initialState)((currentState, nextInstruction) => {
      nextInstruction.apply(currentState, builtMap)
    })

    println(finalState)
    val part1 = score(finalState)
    println(s"Part 1: $part1")
  }

  def parseMap(rawLines: Seq[String]) = {
    implicit def sToI(s: String): Int = Integer.parseInt(s)

    val lineRegex = """^(\s*)([.#]*)(\s*)$""".r

    val processedRows = rawLines.zipWithIndex.map(rowWithIndex => {
      val lineRegex(leadingBlank, mapRow, trailingBlank) = rowWithIndex._1


      val onTheMap = mapRow.toCharArray.zipWithIndex.map(charWithColIdx => {
        MapCoord(Coord(rowWithIndex._2, charWithColIdx._2 + leadingBlank.size), charWithColIdx._1 == '#')
      }).toSeq

      //the bounds should be the coordinate of edge bound valid locations, so that later, there isn't any +/- 1 nonsense
      val rowBounds = MapRowBounds(rowWithIndex._2, leadingBlank.size, leadingBlank.size + mapRow.size - 1)

      (onTheMap, rowBounds)
    })

    //generate MapColBounds
    val validMapLocations = processedRows.map(_._1).flatten.foldLeft(
      (Map[Coord, Boolean]()) //All Coordinates
    )((acc, next) => {
      acc + (next.coord -> next.isRock)
    })

    val colBoundExtremes = processedRows.map(_._2).foldLeft((Int.MaxValue, 0))((acc, next) => {
      (Math.min(acc._1, next.firstCol), Math.max(acc._2, next.lastCol))
    })

    val columnBoundaries = (colBoundExtremes._1 to colBoundExtremes._2).map(col => {
      val columnLocations = validMapLocations.filter(kv => kv._1.col == col)

      val currentColExtremes = columnLocations
        .foldLeft((Int.MaxValue, 0))((acc, next) => {
          (Math.min(acc._1, next._1.row), Math.max(acc._2, next._1.row))
        })
      MapColBounds(col, currentColExtremes._1, currentColExtremes._2)
    })

    val colBoundariesMap = columnBoundaries.map(x => x.col -> x).toMap
    val rowBoundariesMap = processedRows.map(_._2).map(x => x.row -> x).toMap
    MapData(validMapLocations, rowBoundariesMap, colBoundariesMap)
  }

  def parseInstructions(rawInstructionLine: String): Seq[Instruction] = {

    rawInstructionLine
      .replace("R", " R ")
      .replace("L", " L ")
      .trim
      .split(" ")
      .map(token => {
        if (token.equals("R") || token.equals("L")) {
          Instruction(Right(token))
        } else {
          Instruction(Left(Integer.parseInt(token)))
        }
      })
      .toSeq

  }

  def score(endingState: WalkState): Int = {
    val directionScore = endingState.facing.cardinal match {
      case 'N' => 3
      case 'E' => 0
      case 'S' => 1
      case 'W' => 2
    }

    (1000 * (endingState.coord.row + 1)) + (4 * (endingState.coord.col + 1)) + directionScore

  }
}


case class Coord(row: Int, col: Int) {
  def combine(other: Coord): Coord = {
    Coord(row + other.row, col + other.col)
  }
}

case class MapCoord(coord: Coord, isRock: Boolean)

case class MapRowBounds(row: Int, firstCol: Int, lastCol: Int)

case class MapColBounds(col: Int, firstRow: Int, lastRow: Int)

case class MapData(onTheMapCoords: Map[Coord, Boolean], rowBoundsMap: Map[Int, MapRowBounds], colBondsMap: Map[Int, MapColBounds]) {
  def getWrapAroundLocation(offMapCoord: Coord, direction: Direction): Coord = {

    direction.cardinal match {
      case 'N' => {
        val newRow = colBondsMap(offMapCoord.col).lastRow
        Coord(newRow, offMapCoord.col)
      }
      case 'E' => {
        val newCol = rowBoundsMap(offMapCoord.row).firstCol
        Coord(offMapCoord.row, newCol)
      }
      case 'S' => {
        val newRow = colBondsMap(offMapCoord.col).firstRow
        Coord(newRow, offMapCoord.col)
      }
      case 'W' => {
        val newCol = rowBoundsMap(offMapCoord.row).lastCol
        Coord(offMapCoord.row, newCol)
      }
    }

  }
}


case class Instruction(op: Either[Int, String]) {
  def apply(currentWalkState: WalkState, mapData: MapData): WalkState = {
    op match {
      case Left(numOfSteps) => {
        currentWalkState.walk(numOfSteps, mapData)
      }
      case Right(turnDirection) => {
        currentWalkState.turn(shouldTurnLeft = turnDirection.equals("L"))
      }
    }
  }

  override def toString: String = {
    op match {
      case Left(num) => s"I[$num]"
      case Right(v) => s"I[$v]"
    }
  }
}

case class WalkState(coord: Coord, facing: Direction) {
  def turn(shouldTurnLeft: Boolean): WalkState = {
    def newDirection = if (shouldTurnLeft) {
      facing.turnLeft()
    } else {
      facing.turnRight()
    }

    WalkState(this.coord, newDirection)
  }

  def walk(numOfSteps: Int, mapData: MapData): WalkState = {
    (1 to numOfSteps).foldLeft(this)((previousState, stepNum) => {
      val possibleNextCoord = previousState.coord.combine(previousState.facing.getVector())
      mapData.onTheMapCoords.get(possibleNextCoord) match {
        case Some(true) => {
          //hit a rock
          return previousState
        }
        case Some(false) => {
          WalkState(possibleNextCoord, previousState.facing)
        }
        case None => {
          //walked off the edge of the map, to convert possibleNextCoord into the wrapped around point
          val wrapAroundLocation = mapData.getWrapAroundLocation(possibleNextCoord, previousState.facing)
          if (mapData.onTheMapCoords(wrapAroundLocation)) {
            return previousState //hit a rock on the wrap around
          } else {
            WalkState(wrapAroundLocation, previousState.facing) //the wrap around was a rock
          }
        }
      }
    })
  }
}

case class Direction(cardinal: Char) {
  def turnLeft(): Direction = {
    cardinal match {
      case 'N' => Direction('W')
      case 'E' => Direction('N')
      case 'S' => Direction('E')
      case 'W' => Direction('S')
    }
  }

  def turnRight(): Direction = {
    cardinal match {
      case 'N' => Direction('E')
      case 'E' => Direction('S')
      case 'S' => Direction('W')
      case 'W' => Direction('N')
    }
  }

  def getVector(): Coord = {
    cardinal match {
      case 'N' => Coord(-1, 0)
      case 'E' => Coord(0, 1)
      case 'S' => Coord(1, 0)
      case 'W' => Coord(0, -1)
    }
  }
}

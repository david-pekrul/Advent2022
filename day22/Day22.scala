package day22

import day22.Directions.{EAST, NORTH, SOUTH, WEST}
import helpers.Helpers

import scala.annotation.tailrec

object Day22 {
  def main(args: Array[String]): Unit = {
    val rawLines = Helpers.readFile("day22/test.txt")

    val builtMap = parseMap(rawLines.init.init)
    val instructions = parseInstructions(rawLines.last)

    val initialStateCol = builtMap.rowBoundsMap(0).firstCol
    val initialCoord = Coord(0, initialStateCol)
    val initialState = WalkState(initialCoord, Direction('E'))

    val finalState = instructions.foldLeft(initialState)((currentState, nextInstruction) => {
      nextInstruction.apply(currentState, builtMap)
    })

    println(finalState)
    val part1 = score(finalState)
    println(s"Part 1: $part1")


    val squareSize = getSquareSize(builtMap)
    println(s"Square Size: $squareSize")

    val corners = getCorners(builtMap)
    println(s"Corners : $corners")

    val cubeFaces = getCubeFaces(initialCoord, builtMap, squareSize)
    println(s"cubeFaces : $cubeFaces")

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

  def getSquareSize(mapData: MapData): Int = {
    mapData.rowBoundsMap.toSeq.map(_._2).map(row => {
      row.lastCol - row.firstCol
    }).min + 1 //convert from 0 based
  }

  def getCorners(mapData: MapData) = {

    val directions = Seq(NORTH, SOUTH, EAST, WEST)
    val diagonals = Seq(NORTH.getVector().combine(EAST.getVector()), NORTH.getVector().combine(WEST.getVector()), SOUTH.getVector().combine(EAST.getVector()), SOUTH.getVector().combine(WEST.getVector()))

    val insideCorners = mapData.onTheMapCoords.keys.filter(c => {
      directions.forall(d => mapData.onTheMapCoords.contains(c.combine(d.getVector()))) && //all directions exist
        diagonals.exists(dg => !mapData.onTheMapCoords.contains(c.combine(dg))) //there is one diagonal that does not
    }).toSet

    val outsideCorners = mapData.onTheMapCoords.keys.filter(c => {
      directions.filter(d => mapData.onTheMapCoords.contains(c.combine(d.getVector()))).size == 2 //only 2 directions are on the map
    }).toSet

    (insideCorners, outsideCorners)
  }

  def getCubeFaces(initialCoord: Coord, mapData: MapData, squareSize: Int) = {

    val scaledVectors = Directions.ALL.map(_.getVector())

    @tailrec
    def _bfs(currentLayer: Set[(Coord, String)], cubeFaces: Set[CubeFace], adjacencies: Map[(String, Direction), (String, Direction)]): (Set[CubeFace], Map[(String, Direction), (String, Direction)]) = {
      if (currentLayer.isEmpty) {
        return (cubeFaces, adjacencies)
      }
      //convert the current anchor points into cube faces
      val nextCubeFaces = currentLayer.map(c => buildCubeFace(c._1, c._2, mapData, squareSize))

      //get the next anchor points
      val nextLayerLabelVector = currentLayer
        .map(c => {
          val nextFromHere = scaledVectors.map(v => {
            val vNext = v.scale(squareSize).combine(c._1)
            val vector = Directions.fromCoord(v)
            val nextLabel = CubeFace.adjacencyMap((c._2, vector))

            //Label + Direction = Next + Direction.opposite
            //when going from X -> Y, the difference in vectors encodes what the new direction modifier should be
            val oppositeVector = Directions.fromCoord(vector.getVector().opposite)
            val newAdjacency = Seq(
              (c._2, vector) -> (nextLabel, vector),
              (nextLabel, oppositeVector) -> (c._2, oppositeVector)
            ).toMap

            (vNext, nextLabel, vector, newAdjacency)
          })
          nextFromHere
        })
        .flatten
        .filter(c => {
          mapData.onTheMapCoords.contains(c._1)
        })
        .filter(c => {
          !cubeFaces.exists(cf => cf.anchorPoint == c._1)
        })

      val newAdjacencies = nextLayerLabelVector.map(_._4).flatten.toMap

      nextLayerLabelVector.map(next => {
        next._3.getVector()
      })


      val nextLayer = nextLayerLabelVector.map(x => (x._1, x._2))
      _bfs(nextLayer, cubeFaces ++ nextCubeFaces, adjacencies ++ newAdjacencies)
    }

    _bfs(Set((initialCoord, "TOP")), Set.empty, Map.empty)
  }

  def buildCubeFace(anchorPoint: Coord, label: String, mapData: MapData, squareSize: Int) = {
    val faceCoords = (0 to squareSize - 1).flatMap(cubeFaceRow => {
      (0 to squareSize - 1).map(cubeFaceCol => {
        val faceCoord = Coord(cubeFaceRow, cubeFaceCol)
        val originalCoord = anchorPoint.combine(faceCoord)
        CubeFaceCoord(faceCoord, originalCoord, mapData.onTheMapCoords(originalCoord))
      })
    })

    CubeFace(anchorPoint, label, faceCoords.map(f => f -> f.isRock).toMap)
  }

  def mapWraps(insideCorners: Set[Coord], outsideCorners: Set[Coord], mapData: MapData) = {
    //Map[(Coord+Direction) -> Coord+Direction

  }

}


case class Coord(row: Int, col: Int) {
  def combine(other: Coord): Coord = {
    Coord(row + other.row, col + other.col)
  }

  lazy val opposite = Coord(row * -1, col * -1)

  //This is meant to be used for vectors and not points. I'm tool lazy to create another class.
  def scale(scaler: Int) = {
    Coord(row * scaler, col * scaler)
  }
}

//object Vectors {
//  val UP = Coord(-1, 0)
//  val DOWN = Coord(1, 0)
//  val LEFT = Coord(0, -1)
//  val RIGHT = Coord(0, 1)
//}

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

object Directions {
  val NORTH = Direction('N')
  val SOUTH = Direction('S')
  val EAST = Direction('E')
  val WEST = Direction('W')

  val ALL = Seq(NORTH, SOUTH, EAST, WEST)

  def fromCoord(c: Coord): Direction = {
    val result = ALL.find(d => d.getVector() == c)
    result.get
  }
}

case class Direction(cardinal: Char) {
  def turnLeft(): Direction = {
    cardinal match {
      case 'N' => WEST
      case 'E' => NORTH
      case 'S' => EAST
      case 'W' => SOUTH
    }
  }

  def turnRight(): Direction = {
    cardinal match {
      case 'N' => EAST
      case 'E' => SOUTH
      case 'S' => WEST
      case 'W' => NORTH
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

case class CubeFace(anchorPoint: Coord, label: String, facePoints: Map[CubeFaceCoord, Boolean]) {

}

case class CubeFaceCoord(faceCoord: Coord, originalCoord: Coord, isRock: Boolean) {

}

object CubeFace {
  val adjacencyMap = Map(
    ("TOP", NORTH) -> "BACK",
    ("TOP", EAST) -> "RIGHT",
    ("TOP", SOUTH) -> "FRONT",
    ("TOP", WEST) -> "LEFT",

    ("FRONT", NORTH) -> "TOP",
    ("FRONT", EAST) -> "RIGHT",
    ("FRONT", SOUTH) -> "BOTTOM",
    ("FRONT", WEST) -> "LEFT",

    ("BOTTOM", NORTH) -> "FRONT",
    ("BOTTOM", EAST) -> "RIGHT",
    ("BOTTOM", SOUTH) -> "BACK",
    ("BOTTOM", WEST) -> "LEFT",

    ("BACK", NORTH) -> "BOTTOM",
    ("BACK", EAST) -> "RIGHT",
    ("BACK", SOUTH) -> "TOP",
    ("BACK", WEST) -> "LEFT",

    ("RIGHT", NORTH) -> "TOP",
    ("RIGHT", EAST) -> "BACK",
    ("RIGHT", SOUTH) -> "BOTTOM",
    ("RIGHT", WEST) -> "FRONT",

    ("LEFT", NORTH) -> "TOP",
    ("LEFT", EAST) -> "FRONT",
    ("LEFT", SOUTH) -> "BOTTOM",
    ("LEFT", WEST) -> "BACK"
  )
}
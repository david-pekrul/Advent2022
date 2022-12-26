package day15

import helpers.Helpers

object Day15 {
  def main(args: Array[String]): Unit = {

//    val inputs = ("day15/test.txt",10)
    val inputs = ("day15/day15.txt",2000000)

    val sensorsAndBeacons = Helpers.readFile(inputs._1).map(parseLine)

    val part1 = getLocationsWithoutBeacons(inputs._2,sensorsAndBeacons.map(_._1).toSet).size
    println(s"Part 1: $part1")
  }

  def parseLine(line: String): (Sensor, Beacon) = {
    val regex = """(?:-?\d+)""".r
    val matches = regex.findAllMatchIn(line)

    implicit def stringToInt(x: String) = Integer.parseInt(x)

    val sX = matches.next().group(0)
    val sY = matches.next().group(0)
    val bX = matches.next().group(0)
    val bY = matches.next().group(0)
    //x = col, y = row
    val b = Beacon(Coord(bY, bX))
    (Sensor(Coord(sY, sX), b), b)
  }

  def getLocationsWithoutBeacons(yRow: Int, sensors: Set[Sensor]) = {
    val coveredRowRanges = sensors.map(_.getYIntersectCols(yRow)).filter(_.isDefined).map(_.get)
    val corners = getGridCorners(sensors)
    val xRange = (corners._1.col to corners._2.col)

    val beaconlessLocations = coveredRowRanges.foldLeft(Set[Int]())((acc, next) => {
      acc ++ (next._1 to next._2)
    })

    //remove the locations that HAVE a Beacon!
    beaconlessLocations -- sensors.map(_.closestBeacon).filter(_.coord.row == yRow).map(_.coord.row)
  }

  def getGridCorners(sensors: Set[Sensor]) = {
    val beaconCoords = sensors.map(_.closestBeacon.coord)
    val sensorCoords = sensors.map(_.coord)
    val allCoords = beaconCoords ++ sensorCoords
    val startingVals = (allCoords.head.row, allCoords.head.row, allCoords.head.col, allCoords.head.col)

    val extremes = allCoords.foldLeft(startingVals)((acc, next) => {
      (Math.min(acc._1, next.row), Math.max(acc._2, next.row), Math.min(acc._3, next.col), Math.max(acc._4, next.col))
    })

    (Coord(extremes._1, extremes._3), Coord(extremes._2, extremes._4))
  }
}

case class Beacon(coord: Coord)

case class Sensor(coord: Coord, closestBeacon: Beacon) {
  lazy val radius: Int = {
    coord.taxiDistanceTo(closestBeacon.coord)
  }

  def getYIntersectCols(yRow: Int): Option[(Int, Int)] = {
    val a = -1 * radius + Math.abs(coord.row - yRow) + coord.col;
    val b = radius - Math.abs(coord.row - yRow) + coord.col

    val leftYIntercept = Coord(yRow, Math.min(a, b))
    val rightYIntercept = Coord(yRow, Math.max(a, b))

    if (coord.taxiDistanceTo(leftYIntercept) > radius) {
      None
    } else {
      Some((Math.min(a, b), Math.max(a, b)))
    }
  }
}


case class Coord(row: Int, col: Int) {
  def taxiDistanceTo(other: Coord): Int = {
    Math.abs(row - other.row) + Math.abs(col - other.col)
  }
}

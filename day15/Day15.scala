package day15

import helpers.Helpers

object Day15 {
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    //    val inputs = ("day15/test.txt", 10, (0 to 20))
    val inputs = ("day15/day15.txt", 2000000, (0 to 4000000))


    val sensorsAndBeacons = Helpers.readFile(inputs._1).map(parseLine)

    val sensorSet = sensorsAndBeacons.map(_._1)

    val part1BeaconlessRanges = getBeaconlessRanges(inputs._2, sensorSet)
    val part1 = getBeaconlessRangeCount(part1BeaconlessRanges)
    println(s"Part 1: $part1") //5809294

    val distressBeaconLocation = inputs._3.to(LazyList)
      .map(yRow => {
        //map every row to the ranges which beacons cannot exist
        yRow -> getBeaconlessRanges(yRow, sensorSet, false)
      })
      .filter(x => {
        //the location of the distress beacon has to be in a gap in the ranges
        def ranges = x._2
        ranges.size == 2 && ranges.head._2 == ranges.last._1 - 2 //this is the gap!
      })
      .take(1) //since the location is unique, force the Seq to just one element
      .map(x => {
        val row = x._1
        val col = x._2(0)._2 + 1
        Coord(row, col) //the gap location
      })
      .head

    val part2 = (distressBeaconLocation.col.longValue() * 4000000l + distressBeaconLocation.row)

    println(s"Part 2: $part2")

    val end = System.currentTimeMillis()
    println(s"\t[time: ${(end - start) / 1000.0}s]")

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

  def getBeaconlessRanges(yRow: Int, sensors: Seq[Sensor], filterBeaconLocations: Boolean = true) = {
    val sortedCoveredRanges = sensors.map(_.getYIntersectCols(yRow)).filter(_.isDefined).map(_.get).toSeq.sortBy(_._1)

    //head == starter, and foldLeft over the rest
    val sortedRangesWithoutBeacons = sortedCoveredRanges.tail.foldLeft(Seq[(Int, Int)](sortedCoveredRanges.head))((acc, next) => {
      //determine if next overlaps
      //since these are sorted by the left/_._1 values, the next range will never start before one we've seen.
      /*
          cases to merge
          last:   |----------------|
          next1:        |-------|
          next2:        |-------------|
              ->  |-------------------|
          else, there is a gap!
       */
      acc.init
      if (next._1 <= acc.last._2) {
        //the next range starts within the last range
        //Seq.init == all but the last element
        acc.init :+ (acc.last._1, Math.max(acc.last._2, next._2)) //update the last range go from the beginning to the max of either range
      } else {
        //there is a gap!
        acc :+ next
      }
    })

    val beaconLocationsOnRow = if (filterBeaconLocations) {
      sensors.map(_.closestBeacon).filter(_.coord.row == yRow).map(_.coord.col).toSeq.sorted
    } else {
      Seq()
    }

    val beaconlessRanges = beaconLocationsOnRow.foldLeft(sortedRangesWithoutBeacons)((acc, beaconLocation) => {
      //see if this beacon location splits a range
      acc.foldLeft(Seq[(Int, Int)]())((acc2, seq) => {
        if (seq._1 < beaconLocation && seq._2 > beaconLocation) {
          //split this seq, omiting the beacon location
          acc2 ++ Seq((seq._1, beaconLocation - 1), (beaconLocation + 1, seq._2))
        } else {
          acc2 :+ seq
        }
      })
    })
    beaconlessRanges
  }

  def getBeaconlessRangeCount(beaconlessRanges: Seq[(Int, Int)]): Int = {
    val result = beaconlessRanges.foldLeft(0)((acc, next) => {
      acc + (next._2 - next._1) + 1
    })

    result
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

    val leftRowIntercept = Coord(yRow, Math.min(a, b))

    //Checking to make sure that this coordinate is within the radius of this sensor
    if (coord.taxiDistanceTo(leftRowIntercept) > radius) {
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

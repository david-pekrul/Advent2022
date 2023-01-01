package day17

import day17.Rocks.{AT_REST, FALLING, ROCKS}
import helpers.Helpers
import helpers.Helpers.{infiniteIndexedStream, infiniteStream}

import scala.collection.mutable

object Day17 {
  def main(args: Array[String]): Unit = {

    val rawChars = Helpers.readFile("day17/day17.txt").head.toCharArray.toSeq

    def jetsIterator = infiniteStream(rawChars.map(Jet(_))).iterator

    def rocksIterator = infiniteStream(ROCKS).iterator

    def jetsIndexedIterator = infiniteIndexedStream(rawChars.map(Jet(_))).iterator

    def rocksIndexedIterator = infiniteIndexedStream(ROCKS).iterator

    val NUM_ROCKS = 2022
    val endRockPile = run(rocksIterator, jetsIterator, NUM_ROCKS)
    val part1 = getHeight(endRockPile)
    println(s"Part 1: $part1")
    //    printRocks(endRockPile)

    val part2Limit = 1000000000000L
    val (cycleStart, cycleEnd, rowInfo) = findCycleSize(rocksIndexedIterator, jetsIndexedIterator)

    val cycleHeight = cycleStart.height - cycleEnd.height
    val cycleRounds = cycleStart.round - cycleEnd.round
    val numberOfCycles = (part2Limit - cycleEnd.round) / cycleRounds
    val remainingRounds = (part2Limit - cycleEnd.round) % cycleRounds
    val remainingHeight = rowInfo(cycleEnd.round + remainingRounds) - cycleEnd.height

    // |----initial rounds----|~~ cycle1 ~~|~~ cycle2 ~~|~~ cycle3 ~~| -- remaining rounds (if there are any)
    val part2 = (rowInfo(cycleEnd.round)-1) + //don't count the top row of the initial twice, as it is also the last row in the cycle
      (numberOfCycles * cycleHeight) +        //add up the heights of all of the full-height cycles
      remainingHeight                         //add up the remaining height from a partial cycle

    println(s"Part 2: $part2")
  }

  def convertRockPileToCoordSet(rockPile: Seq[Rock]): Set[Coord] = {
    rockPile.foldLeft(Set[Coord]())((acc, rock) => {
      acc ++ rock.getPrintCoords()
    })
  }

  def printRocks(rockPile: Seq[Rock]): Unit = {
    //find bounds
    val (lowBound, upBound) = rockPile.foldLeft((Int.MaxValue.toLong, 0L))((acc, next) => {
      (Math.min(acc._1, next.veryBottom), Math.max(acc._2, next.veryTop))
    })

    val printCoords = convertRockPileToCoordSet(rockPile)
    println()
    (upBound to lowBound by -1).foreach(row => {
      print('|')
      (0 to 6).foreach(col => {
        if (printCoords.contains(Coord(row, col))) {
          print("#")
        } else {
          print(".")
        }
      })
      println(s"| $row")
    })

    println("&&&&&&&&&\r\n")
  }


  def run(rocks: Iterator[Rock], jets: Iterator[Jet], numberOfRocks: Int) = {
    rocks.take(numberOfRocks).foldLeft(Seq[Rock]())((rockPile, currentRockRaw) => {
      val startHeight = getNextStartHeight(rockPile)
      val currentRock = currentRockRaw.appear(startHeight)
      val updatedPile = fallRock(rockPile, jets, currentRock)
//      printRocks(updatedPile)
      updatedPile
    })
  }

  case class RockIdxJetIdx(rockIdx: Int, jetIndex: Int)

  case class RoundAndHeight(round: Long, height: Long)

  case class Accumulator(
                          rockPile: Seq[Rock],
                          rockRows: mutable.Map[Long, Int],
                          stateTracker: mutable.Map[RockIdxJetIdx, mutable.Seq[RoundAndHeight]],
                          maxHeight: Long,
                          round: Long)

  def findCycleSize(rocks: Iterator[(Rock, Int)], jets: Iterator[(Jet, Int)]): (RoundAndHeight,RoundAndHeight,Map[Long, Long]) = {

    //find out how long a cycle is
    rocks.foldLeft(Accumulator(Seq(), mutable.Map(), mutable.Map(), 0L, 0L))((acc, currentRockWithIndex) => {
      val startHeight = getNextStartHeight(acc.rockPile.toSeq)
      val currentRock = currentRockWithIndex._1.appear(startHeight)
      val (updatedPile, jetIdx) = fallRock2(acc.rockPile.toSeq, jets, currentRock)

      val rockIdx = currentRockWithIndex._2

      val mapKey = RockIdxJetIdx(rockIdx, jetIdx)
      val restingRock = updatedPile.head
      val newRestingRockCoords = restingRock.getPrintCoords()
      newRestingRockCoords.foreach(coord => {
        acc.rockRows.update(coord.row, acc.rockRows.getOrElse(coord.row, 0) + columnToBitShift(coord.col))
      })

      val currentRoundAndHeight = RoundAndHeight(acc.round, Math.max(restingRock.veryTop,acc.maxHeight)) //the new rock might be resting below the top of previous rocks

      acc.stateTracker.get(mapKey) match {
        case None => {
          //this Rock+Jet hasn't been seen before, so add it to the map
          acc.stateTracker.update(mapKey, mutable.Seq(currentRoundAndHeight))
        }
        case Some(previousRoundsAndHeights) => {
          if (!previousRoundsAndHeights.isEmpty) {
            compareStates(currentRoundAndHeight, previousRoundsAndHeights, acc.rockRows) match {
              case None => {
                acc.stateTracker.update(mapKey, previousRoundsAndHeights.appended(currentRoundAndHeight))
              }
              case Some(cycleEnds) => {
                /* WE FOUND A CYCLE!*/
                // printRocks(acc.rockPile)
                val (cycleStart, cycleEnd) = cycleEnds
                val allRounds = acc.stateTracker.values.flatten.map(e => e.round -> e.height).toMap
                return (cycleStart, cycleEnd, allRounds)
              }
            }
          }
        }
      }

      Accumulator(
        rockPile = updatedPile,
        rockRows = acc.rockRows,
        stateTracker = acc.stateTracker,
        Math.max(restingRock.veryTop,acc.maxHeight),
        round = acc.round + 1L
      )
    })

    ??? //this only happens when we run out of elements, which it shouldn't.
  }

  def columnToBitShift(col: Int): Int = {
    Math.pow(2, col).intValue()
  }

  def compareStates(top: RoundAndHeight, previous: mutable.Seq[RoundAndHeight], rockRows: mutable.Map[Long, Int]): Option[(RoundAndHeight, RoundAndHeight)] = {
    previous.reverse.foreach(prev => {
      if (compareStates(top, prev, rockRows)) {
        return Some(top, prev)
      }
    })
    None
  }


  def compareStates(top: RoundAndHeight, bottom: RoundAndHeight, rockRows: mutable.Map[Long, Int]): Boolean = {
    val heightDifference = top.height - bottom.height

    if(heightDifference > bottom.height){
      return false //the bottom one isn't tall enough to compare to the top ones
    }

    (heightDifference to 0 by -1).foreach(deltaY => {
      if (rockRows.get(top.height - deltaY) != rockRows.get(bottom.height - deltaY)) {
        return false
      }
    })

    true //there were no differences
  }

  def findCycle(pile: Seq[Rock], map: mutable.Map[Int, Seq[Coord]]) = {

    def topRock = pile.head

    def topRockLocations = map.get(topRock.id).get

    //grouped by col
    //column => rows
    val sortedColGroupings = topRockLocations.groupBy(_.col).map(colGroup => (colGroup._1, colGroup._2.map(_.row).sorted))

    println("Cycle ?")
  }


  def getNextStartHeight(rockPile: Seq[Rock]): Long = {
    getHeight(rockPile) + 4
  }

  def getHeight(rockPile: Seq[Rock]): Long = {
    rockPile.take(10).map(_.veryTop).foldLeft(0L)((acc, next) => {
      Math.max(acc, next)
    })
  }


  def fallRock(rockPile: Seq[Rock], jets: Iterator[Jet], fallingRockStart: Rock): Seq[Rock] = {
    jets.foldLeft(fallingRockStart)((previousRock, nextJet) => {
      //      printRocks(previousRock +: rockPile)
      val currentRock = nextJet.apply(previousRock, rockPile)._1
      //      printRocks(currentRock +: rockPile)
      currentRock.pushDown(rockPile) match {
        case (r, AT_REST) => {
          //          printRocks(r +: rockPile)
          //          print("~~~~REST~~~")
          return (r +: rockPile) //return out of the whole function
        }
        case (r, FALLING) => {
          r
        }
      }
    })
    throw new Exception("Ran out of jets???")
  }

  def fallRock2(rockPile: Seq[Rock], jets: Iterator[(Jet, Int)], fallingRockStart: Rock): (Seq[Rock], Int) = {
    jets.foldLeft((fallingRockStart, Seq[Char]()))((previousRock, nextJet) => {
      //      printRocks(previousRock +: rockPile)
      val currentRock = nextJet._1.apply(previousRock._1, rockPile)
      //      printRocks(currentRock +: rockPile)
      currentRock._1.pushDown(rockPile) match {
        case (r, AT_REST) => {
          //          printRocks(r +: rockPile)
          //          print("~~~~REST~~~")
          //  return the updated rock pile and the index of the sequence index of the last jet
          return (r +: rockPile, nextJet._2) //return out of the whole function
        }
        case (r, FALLING) => {
          (r, previousRock._2 :+ currentRock._2)
        }
      }
    })
    throw new Exception("Ran out of jets???")
  }



}

case class Coord(row: Long, col: Int) {}

case class Jet(symbol: Char) {
  def apply(input: Rock, otherRocks: Seq[Rock]) = {
    symbol match {
      case '>' => {
        //        println("push Right")
        (input.pushRight(otherRocks), symbol)
      }
      case '<' => {
        //        println("push Left")
        (input.pushLeft(otherRocks), symbol)
      }
    }
  }
}

case class Rock(id: Short, bits: Seq[Coord], xShift: Int, yShift: Long) {

  //used for very quick bounds checking
  lazy val veryTop: Long = {
    bits.map(b => {
      b.row + yShift
    }).max
  }

  //used for very quick bounds checking
  lazy val veryBottom: Long = {
    bits.map(b => {
      b.row + yShift
    }).min
  }

  lazy val height = veryTop - veryBottom

  def appear(appearY: Long): Rock = {
    Rock(id, bits, xShift, appearY)
  }

  def pushLeft(existingRocks: Seq[Rock]): Rock = {
    pushX(-1, existingRocks)
  }

  def pushRight(existingRocks: Seq[Rock]): Rock = {
    pushX(1, existingRocks)
  }

  /*
    The resulting Rock
    True => The rock is at rest
    False => The rock is not at rest
   */
  def pushDown(existingRocks: Seq[Rock]): (Rock, Boolean) = {
    val possibleRock = pushY()

    val collisionChecks = existingRocks.take(100) //get enough rocks so that I'm confident this one didn't sneak past any

    for (restingRock <- collisionChecks) {
      if (possibleRock.collides(restingRock)) {
        //        println("Push Down: REST")
        return (this, AT_REST) //return out
      }
    }

    //did not collide with an existing rock; Check the floor
    if (possibleRock.veryBottom == 0) {
      //this new rock went TOO far down
      //      println("Push DOWN: REST")
      (this, AT_REST) //return the current rock and
    } else {
      //      println("Push Down: Fell")
      (possibleRock, FALLING)
    }
  }

  private def pushX(deltaX: Int, otherRocks: Seq[Rock]): Rock = {
    val newXShift = xShift + deltaX
    val shiftedRock = bits.exists(b => b.col + newXShift <= Rocks.LEFT_WALL || b.col + newXShift >= Rocks.RIGHT_WALL) match {
      case true => {
        //        println(s"Pushed X: $deltaX : wall")
        return this
      }
      case false => {
        Rock(id, bits, newXShift, yShift)
      }
    }

    def otherRockSubset = otherRocks.take(100) //take enough to not fall past one

    if (shiftedRock.collides(otherRockSubset)) {
      //      println(s"Pushed X: $deltaX : other rock")
      this
    } else {
      //      println(s"Pushed X: $deltaX")
      shiftedRock
    }
  }

  private def pushY(): Rock = {
    Rock(id, bits, xShift, yShift - 1)
  }

  private def collides(otherRocks: Seq[Rock]): Boolean = {
    otherRocks.exists(other => this.collides(other))
  }

  private def collides(other: Rock): Boolean = {
    if (this.veryBottom > other.veryTop || this.veryTop < other.veryBottom) {
      //can't even overlap
      return false
    }

    val result = this.getPrintCoords.exists(thisBit => {
      other.getPrintCoords.contains(thisBit)
    })
    result //split out for debugging
  }

  def getPrintCoords(): Seq[Coord] = {
    bits.map(bit => {
      Coord(bit.row + yShift, bit.col + xShift)
    })
  }
}


object Rocks {

  val LEFT_WALL = -1
  val RIGHT_WALL = 7
  val AT_REST = true
  val FALLING = false

  val R1: Seq[Coord] = Seq(
    Coord(0, 0), Coord(0, 1), Coord(0, 2), Coord(0, 3)
  )

  val R2: Seq[Coord] = Seq(
    Coord(0, 1), Coord(1, 0), Coord(1, 1), Coord(1, 2), Coord(2, 1)
  )

  val R3: Seq[Coord] = Seq(
    Coord(0, 0), Coord(0, 1), Coord(0, 2), Coord(1, 2), Coord(2, 2)
  )

  val R4: Seq[Coord] = Seq(
    Coord(0, 0), Coord(1, 0), Coord(2, 0), Coord(3, 0)
  )

  val R5: Seq[Coord] = Seq(
    Coord(0, 0), Coord(0, 1), Coord(1, 0), Coord(1, 1)
  )

  val ROCKS = Seq(
    Rock(1, R1, 2, 0),
    Rock(2, R2, 2, 0),
    Rock(3, R3, 2, 0),
    Rock(4, R4, 2, 0),
    Rock(5, R5, 2, 0)
  )
}

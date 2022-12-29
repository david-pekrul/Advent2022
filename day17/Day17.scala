package day17

import day17.Rocks.{AT_REST, FALLING, ROCKS}
import helpers.Helpers

object Day17 {
  def main(args: Array[String]): Unit = {

    val rawChars = Helpers.readFile("day17/day17.txt").head.toCharArray.toSeq
    val jetsIterator = infiniteStream(rawChars.map(Jet(_))).iterator

    val rocksIterator = infiniteStream(ROCKS).iterator

    val NUM_ROCKS = 2022
    val endRockPile = run(rocksIterator, jetsIterator, NUM_ROCKS)

    printRocks(endRockPile)

    val part1 = getHeight(endRockPile)

    println(s"Part 1: $part1")
    //3254 too high
    //3248 too high
  }

  def printRocks(rockPile: Seq[Rock]): Unit = {
    //find bounds
    val (lowBound, upBound) = rockPile.foldLeft((Int.MaxValue.toLong, 0L))((acc, next) => {
      (Math.min(acc._1, next.veryBottom), Math.max(acc._2, next.veryTop))
    })

    val printCoords = rockPile.foldLeft(Set[Coord]())((acc, rock) => {
      acc ++ rock.getPrintCoords()
    })

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
      val currentRock = nextJet.apply(previousRock, rockPile)
      //      printRocks(currentRock +: rockPile)
      currentRock.pushDown(rockPile) match {
        case (r, AT_REST) => {
          //          printRocks(r +: rockPile)
          //          print("~~~~REST~~~")
          return r +: rockPile //return out of the whole function
        }
        case (r, FALLING) => {
          r
        }
      }
    })
    throw new Exception("Ran out of jets???")
  }

  def infiniteStream[A](seed: Seq[A]): LazyList[A] = {
    val x = seed.to(LazyList)

    def xs: LazyList[A] = x #::: infiniteStream(seed)

    xs
  }
}

case class Coord(row: Long, col: Int) {}

case class Jet(symbol: Char) {
  def apply(input: Rock, otherRocks: Seq[Rock]) = {
    symbol match {
      case '>' => {
        //        println("push Right")
        input.pushRight(otherRocks)
      }
      case '<' => {
        //        println("push Left")
        input.pushLeft(otherRocks)
      }
    }
  }
}

case class Rock(bits: Seq[Coord], xShift: Int, yShift: Long) {

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
    Rock(bits, xShift, appearY)
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
        Rock(bits, newXShift, yShift)
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
    Rock(bits, xShift, yShift - 1)
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
    Rock(R1, 2, 0),
    Rock(R2, 2, 0),
    Rock(R3, 2, 0),
    Rock(R4, 2, 0),
    Rock(R5, 2, 0)
  )
}

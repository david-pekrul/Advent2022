package day13

import helpers.Helpers

import java.io.BufferedReader

object Day13 {
  def main(args: Array[String]): Unit = {
    val groupedInput = Helpers.readFile("day13/day13.txt").filter(!_.trim.isBlank).sliding(2, 2)

    val parsedPacketPairs = groupedInput.map(pair => {
      pair.map(Packet.parse)
    }).toSeq

    val part1Processed = parsedPacketPairs.zipWithIndex.map { case (pair, index) => {
      val result = pair(0).comesBefore(pair(1))
      (index + 1, result.get)
    }
    }

    val part1 = part1Processed.filter(_._2).map(_._1).sum
    println(s"Part 1: $part1")


    val dividerPackets = Seq(Packet(Seq(Left(2))), Packet(Seq(Left(6))))
    val part2Start = parsedPacketPairs.flatten ++ dividerPackets

    val part2Sorted = part2Start.sortWith((a, b) => a.comesBefore(b).get)

    val part2 = part2Sorted.zipWithIndex.filter { case (p, index) => {
      dividerPackets.contains(p)
    }
    }.map(_._2+1).reduce((a,b) => a*b)

    println(s"Part 2: $part2")


  }
}

case class Packet(packetData: Seq[Either[Int, Packet]]) {

  def comesBefore(other: Packet): Option[Boolean] = {

    /*
      None => Inconclusive
      Some(true) => are in order
      Some(false) => are out of order
     */
    def areInOrder(leftElemOpt: Option[Either[Int, Packet]], rightElemOpt: Option[Either[Int, Packet]]): Option[Boolean] = {
      if (leftElemOpt.isEmpty && rightElemOpt.isEmpty) {
        return None //inconclusive
      }
      if (leftElemOpt.isEmpty) { //&& rightElemOpt.isDefined
        return Some(true) //left ran of out elements first -> correct order
      }
      if (rightElemOpt.isEmpty) {
        return Some(false) //right ran out of elements first -> incorrect order
      }
      //both are not empty
      (leftElemOpt.get, rightElemOpt.get) match {
        case (Left(lNum), Left(rNum)) => {
          if (lNum > rNum) {
            return Some(false) //
          }
          if (lNum == rNum) {
            return None
          }
          return Some(true)

        }
        case (Right(lPacket), Right(rPacket)) => {
          return lPacket.comesBefore(rPacket);
        }
        case (Right(lPacket), Left(rNum)) => {
          val rightAsPacket = new Packet(Seq(Left(rNum)))
          return lPacket.comesBefore(rightAsPacket)
        }
        case (Left(lNum), Right(rPacket)) => {
          val leftAsPacket = new Packet(Seq(Left(lNum)))
          return leftAsPacket.comesBefore(rPacket)
        }
      }
    }

    //    (0 to Math.max(packetData.size, other.packetData.size) - 1).foreach(index => {
    //      val leftOpt = packetData.lift(index)
    //      val rightOpt = other.packetData.lift(index)
    //      val theseInOrder = areInOrder(leftOpt, rightOpt)
    //      theseInOrder match {
    //        case Some(result) => return Some(result)
    //        case None => None
    //      }
    //    })

    val result2 = (0 to Math.max(packetData.size, other.packetData.size) - 1).foldLeft(None: Option[Boolean])((acc, index) => {
      if (acc.isDefined) {
        acc
      } else {
        val leftOpt = packetData.lift(index)
        val rightOpt = other.packetData.lift(index)
        val result = areInOrder(leftOpt, rightOpt)
        result match {
          case Some(result) => Some(result)
          case None => None
        }
      }
    })

    result2

  }

  override def toString: String = {
    val s = packetData.map(x => {
      x match {
        case Left(num) => num.toString
        case Right(p) => p.toString
      }
    }).mkString(",")
    s"P[$s]"
  }
}

object Packet {
  //https://github.com/j-mie6/Parsley //but I don't want to pull in dependencies! That will make this _work_ work.
  def parse(rawString: String) = {

    val chars = rawString
      .toCharArray.toSeq

    //put the two-digit numbers back together
    val numbersCombinedSeq = chars.foldLeft(Seq[String]())((acc, next) => {
      if (next.isDigit) {
        if (toInt(acc.last) > -1) {
          //the last entry was a digit
          acc.slice(0, acc.size - 1) :+ toInt(acc.last + next).toString
        } else {
          acc :+ next.toString
        }
      } else {
        acc :+ next.toString
      }
    }).filter(!_.equals(","))

    //now only have numbers and brackets
    val eitherSeq = numbersCombinedSeq.foldLeft(Seq[Either[Int, String]]())((acc, next) => {
      if (toInt(next) > -1) {
        acc :+ Left(toInt(next))
      } else {
        acc :+ Right(next)
      }

    })

    def _parse(itr: Iterator[Either[Int, String]]): Packet = {

      val folded = itr.foldLeft(Seq[Packet](Packet(Seq())))((acc, next) => {
        next match {
          case Left(num) => {
            //remove last packet so we can replace it with an updated packet
            val allButLast = acc.reverse.tail.reverse
            val lastPacket = acc.last
            val updatedLastPacket = Packet(lastPacket.packetData :+ Left(num))
            allButLast :+ updatedLastPacket
          }
          case Right("[") => {
            //push a new packet onto the stack
            acc :+ Packet(Seq())
          }
          case Right("]") => {
            //add the last packet into the previous packet
            val lastPacket = acc.last
            val secondToLast = acc(acc.length - 2)
            val allButLastTwo = acc.slice(0, acc.length - 2)
            val newLast = Packet(secondToLast.packetData :+ Right(lastPacket))
            val acc2 = allButLastTwo :+ newLast
            acc2
          }
          case x => {
            throw new RuntimeException(s"Unexpected value: $x")
          }
        }
      })

      //I don't know why I have to dig into this nesting, but ¯\_(ツ)_/¯
      val parsedPacket = folded.head.packetData(0) match {
        case Left(_) => throw new RuntimeException("WHOA!")
        case Right(x) => x
      }

      parsedPacket
    }

    _parse(eitherSeq.iterator)
  }

  def toInt(s: String): Int = {
    try {
      s.toInt
    } catch {
      case e: Exception => -1
    }
  }
}

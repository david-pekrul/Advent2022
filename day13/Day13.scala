package day13

import helpers.Helpers

import java.io.BufferedReader

object Day13 {
  def main(args: Array[String]): Unit = {
    val groupedInput = Helpers.readFile("day13/day13.txt").filter(!_.trim.isBlank).grouped(2)

    val parsedPacketPairs = groupedInput.map(pair => {
      pair.map(Packet.parse)
    }).toSeq

    println(parsedPacketPairs.mkString("\r\n"))

    //todo: Still need part 1, but it is PARSED!
  }
}

case class Packet(seq: Seq[Either[Int, Packet]]) {

  override def toString: String = {
    val s = seq.map(x => {
      x match {
        case Left(num) => num.toString
        case Right(p) => p.toString
      }
    }).mkString(",")
    s"P[$s]"
  }

}

/*
  Packet: '[' + ( + '];

 */

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
            val updatedLastPacket = Packet(lastPacket.seq :+ Left(num))
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
            val newLast = Packet(secondToLast.seq :+ Right(lastPacket))
            val acc2 = allButLastTwo :+ newLast
            acc2
          }
          case x => {
            throw new RuntimeException(s"Unexpected value: $x")
          }
        }
      })

      //I don't know why I have to dig into this nesting, but ¯\_(ツ)_/¯
      val parsedPacket = folded.head.seq(0) match {
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

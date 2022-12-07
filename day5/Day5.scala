package day5

import helpers.Helpers

import java.lang.Integer.parseInt
import scala.collection.mutable.Stack
import scala.collection.mutable.{Seq => MutSeq}

object Day5 {
  def main(args: Array[String]): Unit = {
    val rawLines = Helpers.readFile("day5/day5.txt").toSeq
    val (starter, instructions) = StackParser.parseRawLines(rawLines)
    val part1 = instructions.foldLeft(starter)((a,b) => {
      a.processInstruction(b)
    })

    println(s"Part 1: ${part1.getTops()}")

    val part2 = instructions.foldLeft(starter)((a, b) => {
      a.processInstruction(b,true)
    })
    println(s"Part 2: ${part2.getTops()}")
  }
}

object StackParser {
  def parseRawLines(rawLines: Seq[String]) = {
    //relies on there being no empty columns to start with
    val itr = rawLines.iterator

    val firstPass = itr.takeWhile(!_.isEmpty).filter(_.contains("[")).map(line => {
      val groupedChars = line.toCharArray.toSeq
        .grouped(4).toSeq.map(_.mkString(""))
        .map(x => x.replaceAll("[\\[\\]]", "").trim)
        .zipWithIndex
      groupedChars
    }).flatten.toSeq

    val secondPass = firstPass.groupBy(_._2).toSeq.sortBy(_._1).map(entry => {
     entry._2.map(_._1).filter(!_.isEmpty)
    })

    val instructions = itr.toSeq.map(Instruction(_))

    (Stacks(secondPass), instructions)
  }
}

case class Stacks(currentStack: Seq[Seq[String]]) {
  def processInstruction(op: Instruction, part2: Boolean = false): Stacks = {

    val nextStacks = currentStack.zipWithIndex.map{case (column,idx) => {
      idx match {
        case op.from => column.drop(op.quantity)
        case op.to => {
          val partToMove = currentStack(op.from).take(op.quantity)
          (if(!part2){partToMove.reverse} else {partToMove})  ++ column
        }
        case _ => column
      }
    }}
    Stacks(nextStacks)
  }

  override def toString: String = {
    currentStack.mkString("\r\n\t")
  }

  def getTops(): String = {
    currentStack.map(_.head).mkString("")
  }
}

case class Instruction(rawInput: String) {
  val pattern = "\\d+".r

  lazy val numbers = pattern.findAllIn(rawInput).toSeq.map(parseInt)

  val quantity = numbers(0)

  val from = numbers(1) - 1 //move to 0-based index

  val to = numbers(2) - 1 //move to 0-based index


}



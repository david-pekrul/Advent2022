package day8

import helpers.Helpers

object Day8 {
  def main(args: Array[String]): Unit = {
    def lines = Helpers.readFile("day8/day8.txt")

    val grid = lines.map(line => line.toCharArray.toSeq.map(_.toString.toByte))
    val forrest = grid.map(_.map(b => Tree(b)).toSeq)

    val markedForrest = markForrest(forrest)

    val part1 = markedForrest.flatten.count(_.visible)
    println(s"Part 1: $part1")

  }

  def printGrid[T](grid: Seq[Seq[T]]): Unit = {
    grid.foreach(row => println(row))
  }

  def rotate[T](input: Seq[Seq[T]]): Seq[Seq[T]] = {
    input
      .zipWithIndex
      .map { case (row, rowIndex) => (row.zipWithIndex, rowIndex) }
      .map { case (rowWithColIndexes, rowIndex) => {
        rowWithColIndexes.map(rWithCI => {
          (rowIndex, rWithCI._2, rWithCI._1)
        })
      }
      }
      .flatten
      .groupBy(_._2)
      .toSeq.sortBy(_._1)
      .map(_._2)
      .map(_
        .map {
          case (a, b, c) => (b, c)
        }.sortBy(_._1)
        .map(_._2)
      )
      .toSeq
  }

  def markForrest(forrest: Seq[Seq[Tree]]): Seq[Seq[Tree]] = {
    rotate(markForrestRows(rotate(markForrestRows(forrest))))
  }

  def markForrestRows(forrest: Seq[Seq[Tree]]): Seq[Seq[Tree]] = {
    val firstRow = forrest.head.map(_.makeVisible())
    val lastRow = forrest.last.map(_.makeVisible())
    val middleRows = forrest.slice(1, forrest.size - 1).map(markTrees)

    Seq(firstRow) ++ middleRows ++ Seq(lastRow)
  }

  def markTrees(row: Seq[Tree]): Seq[Tree] = {

    def oneDirectionProcessing(r: Seq[Tree]): Seq[Tree] = {
      val first = r.head
      val last = r.last
      val middle = r.tail.reverse.tail.reverse

      middle.foldLeft(RowAccumulator(first.height, Seq(first.makeVisible())))((acc, next) => {
        if (next.height > acc.min) {
          RowAccumulator(next.height, acc.acc :+ next.makeVisible())
        } else {
          RowAccumulator(acc.min, acc.acc :+ next)
        }
      }).acc :+ last.makeVisible()
    }

    oneDirectionProcessing(oneDirectionProcessing(row).reverse).reverse
  }

  case class RowAccumulator(min: Byte, acc: Seq[Tree])
}

case class Tree(height: Byte, visible: Boolean = false) {
  def makeVisible(): Tree = {
    Tree(height, true)
  }
}



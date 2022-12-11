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

    val forrest2 = forrest.map(_.map(t => RangeTree(t.height)))
    val markedForrest2 = markForrest2(forrest2)

    printGrid(markedForrest2)

    val part2 = markedForrest2.flatten.foldLeft(0l)((acc,next) => Math.max(acc,next.visibleScore))
    println(s"Part 2: $part2")
    //66822 is too low
    //395352 too high
  }

  def printGrid[T](grid: Seq[Seq[T]]): Unit = {
    grid.foreach(row => println(row))
  }

  def reflect[T](input: Seq[Seq[T]]): Seq[Seq[T]] = {
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
    reflect(markForrestRows(reflect(markForrestRows(forrest))))
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

  def markForrest2(forrest: Seq[Seq[RangeTree]]): Seq[Seq[RangeTree]] = {
    reflect(reflect(forrest.map(markTrees2)).map(markTrees2))
  }
  def markTrees2(row: Seq[RangeTree]): Seq[RangeTree] = {
    //these are on the edge, so their score is linked to zero

    def innerOp(r: Seq[RangeTree]): Seq[RangeTree] = {
      val firstTree = RangeTree(r.head.height, 0)
      val lastTree = RangeTree(r.last.height, 0)
      val middleTrees = r.slice(1, r.length - 1)

      middleTrees.foldLeft(RowScoreTracker(firstTree.height, firstTree.height, 1, Seq(firstTree)))((tracker, next) => {
        //TODO: Something is wrong here. I'm getting wrong scores for the second row
        if (next.height > tracker.prevHeight) {
          //this tree is taller than the previous one

          val replacementTree = RangeTree(next.height, next.visibleScore * tracker.seenSinceMax)
          val (newMax, newRunningTotalSinceMax) = if (next.height > tracker.maxSoFar) {
            (next.height, 1)
          } else {
            (tracker.maxSoFar, tracker.seenSinceMax + 1)
          }
          RowScoreTracker(newMax, next.height, newRunningTotalSinceMax, tracker.newRow :+ replacementTree)
        } else {
          //this tree is shorter or equal to the previous one
          val replacementTree = RangeTree(next.height, next.visibleScore)
          RowScoreTracker(tracker.maxSoFar, next.height, tracker.seenSinceMax + 1, tracker.newRow :+ replacementTree)
        }
      })
        .newRow :+ lastTree
    }
    val firstPass = innerOp(row)
    val secondPass = innerOp(firstPass.reverse).reverse
    secondPass
  }

  case class RowAccumulator(min: Byte, acc: Seq[Tree])

  case class RowScoreTracker(maxSoFar: Byte, prevHeight: Byte, seenSinceMax: Int, newRow: Seq[RangeTree])
}

case class Tree(height: Byte, visible: Boolean = false) {
  def makeVisible(): Tree = {
    Tree(height, true)
  }
}

case class RangeTree(height: Byte, visibleScore: Long = 1) {

  override def toString: String = {
    s"T[$height,$visibleScore]"
  }

}



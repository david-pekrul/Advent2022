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
    val markedForrest2 = markForrest4(forrest2)

//    printGrid(markedForrest2)

    val part2 = markedForrest2.flatten.foldLeft(0l)((acc, next) => Math.max(acc, next.visibleScore))
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

  def markForrest4(forrest: Seq[Seq[RangeTree]]): Seq[Seq[RangeTree]] = {
    reflect(reflect(forrest.map(markTrees4)).map(markTrees4))
  }

  def markTrees4(row: Seq[RangeTree]): Seq[RangeTree] = {
    def innerOp(r: Seq[RangeTree]): Seq[RangeTree] = {
      val lastIndexOfHeight: Map[Byte, Int] = Map()
      val firstTree = RangeTree(r.head.height, 0)
      val lastTree = RangeTree(r.last.height, 0)
      val middleTrees = r.slice(1, r.length - 1)
      (firstTree +: middleTrees :+ lastTree).zipWithIndex.foldLeft((lastIndexOfHeight,Seq[RangeTree]()))((acc, currentTreeWithIndex) => {


        val currentIndex = currentTreeWithIndex._2
        val currentTree = currentTreeWithIndex._1
        val blockingIndex = acc._1.filter{case (k,v) => k >= currentTree.height}.map(_._2).maxOption.getOrElse(0)
        val scoreMultiplier = currentIndex - blockingIndex
        val mappedCurrentTree = RangeTree(currentTree.height, currentTree.visibleScore * scoreMultiplier)
        val updatedMap = acc._1.updated(currentTree.height,currentIndex)
        (updatedMap,acc._2 :+ mappedCurrentTree)
      })._2
    }

    val firstPass = innerOp(row)
    val secondPass = innerOp(firstPass.reverse).reverse
    secondPass


  }
  case class RowAccumulator(min: Byte, acc: Seq[Tree])
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




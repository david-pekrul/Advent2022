package day20

import helpers.Helpers

object Day20 {
  def main(args: Array[String]): Unit = {
    //    val testCircleSize = 5000
    //    ((-1*testCircleSize*3) to (testCircleSize*3))
    //      .map(n => Node(n, n))
    //      .map(node => node.n -> node.convertMoveToRightMod(testCircleSize))
    //      .foreach(println)


//    val originalNumbers = Helpers.readFile("day20/test.txt").map(Integer.parseInt)
        val originalNumbers = Helpers.readFile("day20/day20.txt").map(Integer.parseInt)
    //    val originalNumbers = (0 to 5).map(_ * 3)
    //    val originalNumbers = Seq(0, 1, 5, 7, -5, -7)

    val numbersWithIds = originalNumbers.zipWithIndex.map(x => Node(x._1, x._2))
    val numbersSize = originalNumbers.size

    numbersWithIds.sliding(2, 1).foreach(nodePair => {
      nodePair(1).setLeft(nodePair(0))
    })
    numbersWithIds.head.setLeft(numbersWithIds.last)

    val nodesInOperationOrder = numbersWithIds.toSeq.sortBy(_.idx)

    //    println(nodesInOperationOrder.mkString("\r\n"))
    //    println("~~~~~~~~~~~~~~~~~~")

    nodesInOperationOrder.foreach(x => {
      x.move(numbersSize)
      //      println(s"Moving: ${x.n}")
      //      println(nodesInOperationOrder.mkString("\r\n"))
      //      println("~~~~~~~~~~~~~~~~~~")
    })
    //    nodesInOperationOrder.head.move(numbersSize)

    //    println(nodesInOperationOrder.mkString("\r\n"))

    val zeroNode = nodesInOperationOrder.find(_.n == 0).get
    val thousandsPlaces = Seq(1000, 2000, 3000).map(step => zeroNode.getNodeSoManyRight(step).n)
    val part1 = thousandsPlaces.sum
    println(s"Part 1: $part1")
    //13343 too high
    //1319 too low
    //13188 no
    //9914 no


  }
}

case class Node(n: Int, idx: Int) {
  var _left: Option[Node] = None
  var _right: Option[Node] = None

  def right = _right.get

  def left = _left.get

  def setLeft(left: Node): Unit = {
    this._left = Some(left)
    left._right = Some(this)
  }

  def getNodeSoManyRight(step: Int) = {
    (1 to step).foldLeft(this)((currentNode, moveNumber) => {
      currentNode.right
    })
  }

  def move(circleSize: Int) = {
    if (n > 0) {
      moveRight(circleSize)
    }
    if (n < 0) {
      moveLeft(circleSize)
    }
  }

  def moveRight(circleSize: Int): Unit = {
    //    val moveRightDistance = convertMoveToRightMod(circleSize)
    val moveRightDistance = n //% circleSize
    if (moveRightDistance == 0) {
      return
    }
    //take this out of the loop
    val oldLeft = this.left
    val oldRight = this.right
    oldLeft._right = Some(oldRight)
    oldRight._left = Some(oldLeft)

    //this.left.right and this.right are now the same
    val newLeft = (1 to moveRightDistance).foldLeft(this.left)((currentNode, moveNumber) => {
      currentNode.right
    })

    val newLeftsOldRight = newLeft.right

    this._left = Some(newLeft)
    newLeft._right = Some(this)

    newLeftsOldRight._left = Some(this)
    this._right = Some(newLeftsOldRight)
  }

  def moveLeft(circleSize: Int): Unit = {
    //    val moveRightDistance = convertMoveToRightMod(circleSize)
    val moveLeftDistance = n //% circleSize

    if (moveLeftDistance == 0) {
      return
    }

    val oldLeft = this.left
    val oldRight = this.right
    oldLeft._right = Some(oldRight)
    oldRight._left = Some(oldLeft)

    //this.right.left and this.left are now the same
    val newRight = (moveLeftDistance to -1).foldLeft(this.right)((currentNode, moveNumber) => {
      currentNode.left
    })

    val newRightsOldLeft = newRight.left

    this._right = Some(newRight)
    newRight._left = Some(this)

    newRightsOldLeft._right = Some(this)
    this._left = Some(newRightsOldLeft)
  }

  def convertMoveToRightMod(circleSize: Int): Int = {
    val modDistance = n % circleSize

    modDistance match {
      case 0 => 0
      case x if x < 0 => x + circleSize - 1
      case x => x % (circleSize - 1)
    }
  }

  override def toString: String = {
    s"($n,$idx): L:(${left.n}), R:(${right.n})"

  }

}

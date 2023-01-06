package day20

import helpers.Helpers

import scala.collection.mutable

object Day20 {
  def main(args: Array[String]): Unit = {
    //    val originalNumbers = Helpers.readFile("day20/test.txt").map(Integer.parseInt).map(_.toLong)
    val originalNumbers = Helpers.readFile("day20/day20.txt").map(Integer.parseInt).map(_.toLong)

    val part1 = run(originalNumbers, (x: Node, size: Int) => x.move(size))
    println(s"Part 1: ${part1._1}") //1591 is correct

    val part1_1 = run(originalNumbers, (x: Node, size: Int) => x.move2(size))
    println(s"Part 1_1: ${part1_1._1}") //1591 is correct

    //this validates that both seq are the same
    //    part1._2.zip(part1_1._2).zipWithIndex.foreach(pair => {
    //      if (pair._1._1 != pair._1._2) {
    //        println("WRONG!")
    //        println(pair._2)
    //        println(pair._1._1)
    //        println(pair._1._2)
    //        return
    //      }
    //    })

    val decryptionKey = 811589153L
    val part2Original = originalNumbers.map(x => x * decryptionKey)
    val part2 = run(part2Original, (x: Node, size: Int) => x.move2(size), loops = 10)
    println(s"Part 2: ${part2._1}") //1591 is correct
  }

  def run(originalNumbers: Seq[Long], func: (Node, Int) => Unit, loops: Int = 1): (Long, Seq[String]) = {
    val numbersWithIds = originalNumbers.zipWithIndex.map(x => Node(x._1, x._2))
    val circleSize = originalNumbers.size

    numbersWithIds.sliding(2, 1).foreach(nodePair => {
      nodePair(1).setLeft(nodePair(0))
    })
    numbersWithIds.head.setLeft(numbersWithIds.last)

    val nodesInOperationOrder = numbersWithIds.sortBy(_.idx)

    var statesInOrder: Seq[String] = Seq()

    (1 to loops).foreach(loopNum => {
      nodesInOperationOrder.foreach(x => {
        func(x, circleSize)
      })
      //      println("loop: " + loopNum)
      //      println(nodesInOperationOrder.mkString("\r\n"))
      //      statesInOrder = statesInOrder :+ nodesInOperationOrder.toString() //this is for debugging between No-Mod and Use-Mod
      //      println("~~~~~~~~~~~~~~~~~~~~~")
    })

    val zeroNode = nodesInOperationOrder.find(_.n == 0).get
    val thousandsPlaces = Seq(1000, 2000, 3000).map(step => zeroNode.getNodeSoManyRight(step).n)
    println(thousandsPlaces)
    (thousandsPlaces.sum, statesInOrder)
  }
}

case class Node(n: Long, idx: Int) {
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

  def move2(circleSize: Int) = {
    if (n > 0) {
      moveRight(circleSize, true)
    }
    if (n < 0) {
      moveLeft(circleSize, true)
    }
  }


  def moveRight(circleSize: Int, useMod: Boolean = false): Unit = {

    val moveRightDistance = if (useMod) {
      loopMod(circleSize)
    } else {
      n.intValue()
    }
    if (moveRightDistance == 0) {
      return
    }
    //take this out of the loop
    val oldLeft = this.left
    val oldRight = this.right
    oldLeft._right = Some(oldRight)
    oldRight._left = Some(oldLeft)

    //this.left.right and this.right are now the same
    var counter = 0
    val newLeft = (1 to moveRightDistance).foldLeft(this.left)((currentNode, moveNumber) => {
      counter += 1
      currentNode.right
    })

    val newLeftsOldRight = newLeft.right

    this._left = Some(newLeft)
    newLeft._right = Some(this)

    newLeftsOldRight._left = Some(this)
    this._right = Some(newLeftsOldRight)
  }

  def moveLeft(circleSize: Int, useMod: Boolean = false): Unit = {
    val moveLeftDistance = if (useMod) {
      loopMod(circleSize)
    } else {
      n.intValue()
    }

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

  def loopMod(circleSize: Int): Int = {

    if (n == 0) {
      return 0 //zero is the only one that does not modify the chain
    }
    val modDistance = (n % circleSize).intValue()

    //how many times does this element end up at the same INDEX, but because of swapping places, everything ELSE moves 1 "back", aka: this one moves FORWARD 1.
    val loops = (((n) / circleSize) % (circleSize - 1)).intValue()

    return modDistance + loops


    /*
    //Old code from part 1
    //before I realized that moving "Mod Zero" spaces actually did move things
    if (modDistance == 0) {
      //this is not zero!
      //the element takes back it's original spot, however, the first move put it's right element in that spot.
      //[Non-Zero] % circleSize == 0 moves the element from "in front" of this node to "behind"
      //      return 0
      if (n > 0) {
        return loops
      } else {
        return -1 * loops
      }
    }

    if (modDistance > 0) {
      return modDistance + loops
    } else {
      return modDistance - loops
    }*/
  }

  override def toString: String = {
    s"($n,$idx): L:(${left.n}), R:(${right.n})"
  }

}
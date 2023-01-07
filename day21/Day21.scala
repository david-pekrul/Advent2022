package day21

import helpers.Helpers

import scala.annotation.tailrec

object Day21 {
  def main(args: Array[String]): Unit = {
    val rawLines = Helpers.readFile("day21/day21.txt")

    val monkeyMap = rawLines
      .map(parse)
      .foldLeft(Map[String, Monkey]())((acc, monkey) => {
        acc + (monkey.id -> monkey)
      })

    val rootMonkey = monkeyMap("root")

    val part1 = calc(rootMonkey, monkeyMap)
    println(s"Part 1: ${part1._1}")

    val part2Ish = calc2(rootMonkey, monkeyMap)
    val part2 = computeEqual(part2Ish._1.right.get, part2Ish._2)
    println(s"Part 2: ${part2}")

    val part2Check = calc2(rootMonkey, monkeyMap, humanValue = Some(part2))

    println(s"\r\nDo the left and right match using 'humn'=${part2}?")
    println(s"Part2 Check: ${part2Check._2(part2Check._2("root").rightId.get).num.get}  ?=  ${part2Check._2(part2Check._2("root").leftId.get).num.get}")
  }

  def calc(root: Monkey, monkeyMap: Map[String, Monkey]) = {


    def _dfs(currentMonkey: Monkey, updatedMap: Map[String, Monkey]): (Long, Map[String, Monkey]) = {
      if (currentMonkey.num.isDefined) {
        (currentMonkey.num.get, updatedMap)
      } else {
        //get left value
        val (leftVal, leftUpdatedMap) = _dfs(updatedMap(currentMonkey.leftId.get), updatedMap)
        val (rightVal, rightUpdatedMap) = _dfs(leftUpdatedMap(currentMonkey.rightId.get), leftUpdatedMap)
        val (computedVal, updatedCurrent) = currentMonkey.performOp(leftVal, rightVal)

        val updatedMapAfterCurrent = rightUpdatedMap.updated(updatedCurrent.id, updatedCurrent)
        (computedVal, updatedMapAfterCurrent)
      }
    }

    _dfs(root, monkeyMap)
  }

  def calc2(root: Monkey, monkeyMap: Map[String, Monkey], humanValue: Option[Long] = None) = {

    def _dfs(currentMonkey: Monkey, updatedMap: Map[String, Monkey]): (Either[Long, Monkey], Map[String, Monkey]) = {
      if (currentMonkey.id.equals("humn")) {
        if (humanValue.isEmpty) {
          (Right(currentMonkey), updatedMap)
        } else {
          val h = Monkey(currentMonkey.id, num = humanValue)
          val u2 = updatedMap.updated("humn", h)
          (Left(humanValue.get), u2)
        }
      } else if (currentMonkey.num.isDefined) {
        (Left(currentMonkey.num.get), updatedMap)
      } else {
        //get left value
        val (leftVal, leftUpdatedMap) = _dfs(updatedMap(currentMonkey.leftId.get), updatedMap)
        val (rightVal, rightUpdatedMap) = _dfs(leftUpdatedMap(currentMonkey.rightId.get), leftUpdatedMap)
        val (computedValOpt, updatedCurrent) = currentMonkey.performOp2(leftVal, rightVal)

        val updatedMapAfterCurrent = rightUpdatedMap.updated(updatedCurrent.id, updatedCurrent)
        val x = computedValOpt match {
          case Some(v) => Left(v)
          case None => Right(updatedCurrent)
        }
        (x, updatedMapAfterCurrent)
      }
    }

    _dfs(root, monkeyMap)
  }

  def computeEqual(root: Monkey, monkeyMap: Map[String, Monkey]): Long = {

    @tailrec
    def _compute(current: Monkey, result: Long): Long = {
      if (current.id.equals("humn")) {
        return result
      }
      val (newUnknownMonkey, newResult) = current.unperformOp(monkeyMap, result)
      _compute(newUnknownMonkey, newResult)
    }

    val left = monkeyMap(root.leftId.get)
    val right = monkeyMap(root.rightId.get)

    if (left.num.isDefined) {
      _compute(right, left.num.get)
    } else {
      _compute(left, right.num.get)
    }
  }


  def parse(rawLine: String) = {
    val operationRegex = """(\w{4}): (\w{4}) ([\+\-\*\/]) (\w{4})""".r
    val numberRegex = """(\w{4}): (\d+)""".r

    implicit def sToI(s: String) = Integer.parseInt(s)

    if (operationRegex.matches(rawLine)) {
      val operationRegex(id, left, op, right) = rawLine
      Monkey(id, leftId = Some(left), rightId = Some(right), op = Some(op))
    } else {
      val numberRegex(id, num) = rawLine
      Monkey(id, num = Some(num * 1L))
    }
  }
}

case class Monkey(id: String,
                  leftId: Option[String] = None,
                  rightId: Option[String] = None,
                  op: Option[String] = None,
                  num: Option[Long] = None) {

  def performOp(leftVal: Long, rightVal: Long): (Long, Monkey) = {
    val computedNum = op match {
      case None => throw new RuntimeException("No operation defined")
      case Some("*") => leftVal * rightVal
      case Some("+") => leftVal + rightVal
      case Some("-") => leftVal - rightVal
      case Some("/") => leftVal / rightVal
      case Some(x) => throw new RuntimeException(s"Unrecognized $x")
    }

    (computedNum, Monkey(id = id, leftId = leftId, rightId = rightId, op = op, num = Some(computedNum)))
  }

  def unperformOp(monkeyMap: Map[String, Monkey], result: Long): (Monkey, Long) = {

    val leftMonkey = monkeyMap(leftId.get)
    val rightMonkey = monkeyMap(rightId.get)

    //    print(s"$this\t\t; $result")

    // leftVal _op_ rightVal = result
    if (rightMonkey.num.isDefined) {
      val rightNum = rightMonkey.num.get
      //      println(s"\t\t$rightNum")
      val computedNum = op match {
        case None => throw new RuntimeException("No operation defined")
        case Some("*") => result / rightNum //leftVal * rightNum = result
        case Some("+") => result - rightNum //left + right = result
        case Some("-") => result + rightNum //left - right = result
        case Some("/") => result * rightNum //left / right = result
        case Some(x) => throw new RuntimeException(s"Unrecognized $x")
      }
      return (leftMonkey, computedNum)
    } else {

      val leftNum = leftMonkey.num.get
      //      println(s"\t\t$leftNum")

      val computedNum = op match {
        case None => throw new RuntimeException("No operation defined")
        case Some("*") => result / leftNum //leftVal * rightNum = result
        case Some("+") => result - leftNum //left + right = result
        case Some("-") => leftNum - result //left - right = result
        case Some("/") => leftNum / result //left / right = result
        case Some(x) => throw new RuntimeException(s"Unrecognized $x")
      }
      return (rightMonkey, computedNum)
    }

  }

  def performOp2(_leftVal: Either[Long, Monkey], _rightVal: Either[Long, Monkey]): (Option[Long], Monkey) = {

    if (_leftVal.isRight || _rightVal.isRight) {
      return (None, this)
    }

    def leftVal = _leftVal.left.get

    def rightVal = _rightVal.left.get

    val computedNum = op match {
      case None => throw new RuntimeException("No operation defined")
      case Some("*") => leftVal * rightVal
      case Some("+") => leftVal + rightVal
      case Some("-") => leftVal - rightVal
      case Some("/") => leftVal / rightVal
      case Some(x) => throw new RuntimeException(s"Unrecognized $x")
    }

    (Some(computedNum), Monkey(id = id, leftId = leftId, rightId = rightId, op = op, num = Some(computedNum)))
  }
}
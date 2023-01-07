package day21

import helpers.Helpers

object Day21 {
  def main(args: Array[String]): Unit = {
    //    val rawLines = Helpers.readFile("day21/test.txt")
    val rawLines = Helpers.readFile("day21/day21.txt")

    val monkeyMap = rawLines
      .map(parse)
      .foldLeft(Map[String, Monkey]())((acc, monkey) => {
        acc + (monkey.id -> monkey)
      })

    //    println(monkeyMap.mkString("\r\n"))

    val rootMonkey = monkeyMap("root")

    val part1 = calc(rootMonkey, monkeyMap)
    println(s"Part 1: ${part1._1}")
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


}
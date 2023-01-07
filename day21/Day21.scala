package day21

import helpers.Helpers

object Day21 {
  def main(args: Array[String]): Unit = {
    val rawLines = Helpers.readFile("day21/test.txt")

    val monkeyMap = rawLines
      .map(parse)
      .foldLeft(Map[String, Monkey]())((acc, monkey) => {
        acc + (monkey.id -> monkey)
      })

    println(monkeyMap.mkString("\r\n"))

    val rootMonkey = monkeyMap("root")



  }

  def putMonkeysIntoTree(monkeyMap: Map[String, Monkey]) = {

    def _dfs()

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
      Monkey(id, num = Some(num))
    }
  }
}

case class Monkey(id: String,
                  leftId: Option[String] = None,
                  rightId: Option[String] = None,
                  op: Option[String] = None,
                  num: Option[Int] = None) {

}
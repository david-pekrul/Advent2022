package day11

import helpers.Helpers

import scala.collection.immutable.HashMap
import scala.collection.mutable

object Day11 {
  def main(args: Array[String]): Unit = {
    val inputIterator = Helpers.readFile("day11/day11.txt").iterator

    val monkeys = Monkey.parseMonkeys(inputIterator)

//    println(monkeys.mkString("\r\n"))
    val after20Rounds = (1 to 20).foldLeft(monkeys)((prevMonkeys,loopCount) => {
      Monkey.playRound(prevMonkeys)
    })

//    printMonkeys(after20Rounds)

    val part1 = after20Rounds.values.toSeq.map(_.inspectCount).sorted.reverse.slice(0,2).reduce((a,b) => a*b)
    println(s"Part 1: ${part1}")
  }

  def printMonkeys(m: Map[Int, Monkey]): Unit = {
    println()
    m.values.toSeq.sortBy(_.id).map(m => (m.id -> m.items, m.inspectCount)).foreach(println)
  }

}

case class Monkey(id: Int, items: Seq[Int], operation: Int => Int, test: Int => Boolean, trueMonkey: Int, falseMonkey: Int, inspectCount: Int = 0) {
  def addItems(newItems: Seq[Int]): Monkey = {
    Monkey(id,items ++ newItems, operation, test, trueMonkey, falseMonkey, inspectCount)
  }

  def removeItems(): Monkey = {
    Monkey(id,Seq(), operation, test, trueMonkey, falseMonkey, inspectCount + items.size)
  }

  def playTurn(worryDivisor: Int): Map[Int, Seq[Int]] = {
    items
      .map(item => {
        val newWorryLevel = operation(item) / worryDivisor
        val updatedMonkey = if (test(newWorryLevel)) {
          trueMonkey -> newWorryLevel
        } else {
          falseMonkey -> newWorryLevel
        }
        updatedMonkey
      })
      .foldLeft(Map[Int, Seq[Int]]())((acc, next) => {
        acc.updated(next._1, acc.getOrElse(next._1, Seq()) :+ next._2)
      })
  }
}

object Monkey {
  def parseMonkeys(input: Iterator[String]): Map[Int, Monkey] = {
    var monkeys: Seq[Monkey] = Seq()
    while (input.hasNext) {
      val parsedMonkey = parseMonkey(input)
      if (parsedMonkey.isDefined) {
        monkeys = monkeys :+ parsedMonkey.get
      }
    }

    monkeys.map(m => m.id -> m).toMap
  }

  def parseMonkey(input: Iterator[String]): Option[Monkey] = {
    val monkeyIdRaw = input.next()
    if (monkeyIdRaw.trim().isEmpty) {
      return None
    }
    val startingRaw = input.next()
    val operationRaw = input.next()
    val testRaw = input.next()
    val trueRaw = input.next()
    val falseRaw = input.next()

    val monkeyIdPattern = "Monkey (\\d+):".r
    val monkeyIdPattern(monkeyId) = monkeyIdRaw

    val startingItems: Seq[Int] = startingRaw.replace("Starting items: ", "").split(", ").toSeq.map(x => Integer.parseInt(x.trim))

    //    val operationPattern = "([\\*\\+])\\s(\\d+)$".r
    //    val operationPattern(symbol,opNumString) = operationRaw
    val opsSplit = operationRaw.replace("Operation: new = old ", "").trim().split(" ")
    val operation: Int => Int = opsSplit(0) match {
      case "*" => (x: Int) =>
        x * (opsSplit(1) match {
          case "old" => x
          case _ => Integer.parseInt(opsSplit(1))
        })
      case "+" => (x: Int) =>
        x + (opsSplit(1) match {
          case "old" => x
          case _ => Integer.parseInt(opsSplit(1))
        })
    }

    val numberPattern = """(\d+)""".r
    val modValue = Integer.parseInt(numberPattern.findFirstIn(testRaw).get)

    //    val testStripped = testRaw.replace("Test: divisible by ","").trim
    //    val modValue = Integer.parseInt(testStripped)
    val testOperation: Int => Boolean = (x: Int) => x % modValue == 0

    //    val numberPattern(trueMonkeyString) = trueRaw
    //    val numberPattern(falseMonkeyString) = falseRaw

    val trueMonkey = Integer.parseInt(numberPattern.findFirstIn(trueRaw).get)
    val falseMonkey = Integer.parseInt(numberPattern.findFirstIn(falseRaw).get)

    Some(Monkey(
      Integer.parseInt(monkeyId),
      startingItems,
      operation,
      testOperation,
      trueMonkey,
      falseMonkey
    ))
  }

  def playRound(prevMonkeys: Map[Int, Monkey], worryDivisor: Int = 3): Map[Int, Monkey] = {

    prevMonkeys.keys.toSeq.sorted.foldLeft(prevMonkeys)((collector,currentId) => {
      val updatedPlacements = collector.get(currentId).get.playTurn(worryDivisor)
      val updatedMonkeys = updatedPlacements.foldLeft(collector)((c2,u2) => {
        c2
          .updated(u2._1,c2.get(u2._1).get.addItems(u2._2)) //update all the other monkeys
      })
      updatedMonkeys
        .updated(currentId, collector.get(currentId).get.removeItems()) //remove the items this monkey has processed
    })
  }

}

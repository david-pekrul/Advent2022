package day2

import day2.HandThrow.{Paper, Rock, Scissors}
import day2.Outcome.{Lose, Tie, Win}

class Game(val rawInput: String) {

  def getScore() = {
    val parsed = rawInput.split("\r\n").filter(!_.isEmpty).map(parseLine).toSeq
    val played = parsed.map(scoreGame)
    val scored = played.map(outcome => outcome._1.id + outcome._2.id)
    scored.sum
  }

  def getScorePart2() = {
    val parsed = rawInput.split("\r\n").filter(!_.isEmpty).map(parseLine2).toSeq
    val played = parsed.map(scoreGame2)
    val scored = played.map(outcome => outcome._1.id + outcome._2.id)
    scored.sum
  }

  def parseLine(line: String) = {
    val split = line.split(" ")
    (HandThrow.parseThrow(split(0)), HandThrow.parseThrow(split {
      1
    }))
  }

  def parseLine2(line: String) = {
    val split = line.split(" ")
    (HandThrow.parseThrow(split(0)), HandThrow.parseCondition(split {
      1
    }))
  }

  def scoreGame(input: (HandThrow.Value, HandThrow.Value)): (HandThrow.Value, Outcome.Value) = {
    if (input._1 == input._2) {
      return (input._2, Outcome.Tie)
    }
    //input2 == you
    //input1 = opponent
    val outcome = input._2 match {
      case Rock => {
        input._1 match {
          case Paper => Lose
          case Scissors => Win
        }
      }
      case Paper => {
        input._1 match {
          case Scissors => Lose
          case Rock => Win
        }
      }
      case Scissors => {
        input._1 match {
          case Rock => Lose
          case Paper => Win
        }
      }
    }
    (input._2, outcome)
  }

  def scoreGame2(input: (HandThrow.Value, Outcome.Value)): (HandThrow.Value, Outcome.Value) = {
    if (input._2 == Tie) {
      return (input._1, Outcome.Tie)
    }
    //input2 == needed outcome
    //input1 = opponent
    val yourHand = input._1 match {
      case Rock => {
        input._2 match {
          case Win => Paper
          case Lose => Scissors
        }
      }
      case Paper => {
        input._2 match {
          case Win => Scissors
          case Lose => Rock
        }
      }
      case Scissors => {
        input._2 match {
          case Win => Rock
          case Lose => Paper
        }
      }
    }
    (yourHand, input._2)
  }
}

object HandThrow extends Enumeration {
  val Rock = Value(1)
  val Paper = Value(2)
  val Scissors = Value(3)

  def parseThrow(input: String) = {
    input match {
      case "A" | "X" => Rock
      case "B" | "Y" => Paper
      case "C" | "Z" => Scissors
    }
  }

  def parseCondition(input: String): Outcome.Value = {
    input match {
      case "X" => Lose
      case "Y" => Tie
      case "Z" => Win
    }
  }
}

object Outcome extends Enumeration {
  val Lose = Value(0)
  val Tie = Value(3)
  val Win = Value(6)
}




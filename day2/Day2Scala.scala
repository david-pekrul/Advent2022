package day2

object Day2Scala {
  def main(args: Array[String]): Unit = {
    val part1 = (new Game(Day2Input.real)).getScore()
    println(s"Part 1: $part1")
    val part2 = (new Game(Day2Input.real)).getScorePart2()
    println(s"Part 2: $part2")


  }
}

object Day2Input {
  val test =
    """
      |A Y
      |B X
      |C Z""".stripMargin

  val real =
    """A Z
      |A Z
      |A Z
      |B Z
      |C X
      |A Z
      |A Z
      |C Y
      |A Z
      |A Y
      |B Y
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |A Z
      |A Y
      |A Z
      |A Z
      |C Y
      |C X
      |C X
      |C X
      |A Z
      |A Z
      |B Y
      |A Z
      |A Z
      |C Z
      |B Z
      |A Y
      |A Z
      |B Y
      |A Z
      |B Y
      |A X
      |B Z
      |A Z
      |A Z
      |A Z
      |C X
      |C X
      |A X
      |A Z
      |A Z
      |C X
      |A Z
      |B Y
      |A Z
      |B Z
      |A Z
      |A Z
      |B Z
      |B Z
      |C Y
      |B Z
      |A X
      |B Z
      |A Z
      |B Y
      |A Z
      |B Z
      |C X
      |A Z
      |B Y
      |B Z
      |A Z
      |B Y
      |C X
      |B Y
      |A Z
      |A Z
      |A Z
      |B Y
      |A Z
      |A Z
      |B Y
      |B Z
      |C Z
      |A X
      |A Z
      |A Z
      |C Z
      |C X
      |A Z
      |C X
      |A Z
      |A X
      |A Z
      |A Z
      |A Z
      |B X
      |B Y
      |A Z
      |A Z
      |A Z
      |C Y
      |B Y
      |C X
      |C X
      |A Z
      |A Z
      |A Z
      |A X
      |C X
      |B Z
      |C X
      |C X
      |A Z
      |A Z
      |A Z
      |A Z
      |B Z
      |A Y
      |A Y
      |C X
      |A Y
      |C X
      |B Y
      |A Z
      |C Z
      |A Z
      |B Y
      |A Z
      |C X
      |B X
      |A Y
      |C X
      |A Z
      |A Z
      |A Y
      |B Z
      |C X
      |C X
      |C X
      |B Y
      |A Z
      |A Z
      |B Y
      |A Z
      |B Z
      |C X
      |C Z
      |A Z
      |C X
      |B Z
      |A Z
      |B Y
      |C X
      |A Y
      |B Y
      |C X
      |C X
      |A Z
      |A Y
      |A Z
      |A Z
      |A Z
      |A Y
      |C X
      |A Z
      |A Z
      |B Z
      |A Z
      |B Z
      |A Z
      |C Z
      |C Z
      |A Z
      |A Z
      |B Z
      |A Z
      |B Z
      |A Z
      |B Z
      |B Y
      |B Y
      |B Z
      |B Y
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |C X
      |C X
      |A Z
      |A Z
      |C Y
      |A Z
      |C Y
      |B Y
      |C X
      |B Z
      |A Z
      |A Z
      |C X
      |A Z
      |B Y
      |A Z
      |A Z
      |A Z
      |B Z
      |A Z
      |B Y
      |A Z
      |B Y
      |A Z
      |A Z
      |C Z
      |A Z
      |C X
      |C Y
      |C X
      |C Y
      |C X
      |A Z
      |B Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |B Y
      |A Z
      |C X
      |C X
      |A Z
      |A Z
      |C Z
      |C Z
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |A Y
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |C X
      |C X
      |A Z
      |C Y
      |A X
      |A Z
      |A Z
      |A Y
      |B Z
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |B Z
      |B Z
      |A Y
      |B Z
      |C X
      |B Y
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |B Z
      |C X
      |A Z
      |A Y
      |A Z
      |B Y
      |B Z
      |C Z
      |A Z
      |A Y
      |A Z
      |B Z
      |A Y
      |C X
      |A Z
      |A Y
      |A Z
      |C Y
      |A Z
      |A Z
      |B Y
      |C X
      |A Z
      |A Z
      |A X
      |A Z
      |C X
      |A Z
      |B Y
      |A Z
      |C X
      |A Z
      |B Y
      |A Z
      |C Y
      |B Z
      |C X
      |A Z
      |C X
      |B Y
      |A Z
      |C X
      |A Z
      |B Y
      |A Z
      |A Z
      |C Z
      |A Z
      |A Y
      |A Z
      |A Z
      |C X
      |A Y
      |A Z
      |B Z
      |A Z
      |A Y
      |C X
      |A Z
      |B Y
      |A Y
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |B Z
      |B Y
      |B X
      |C Y
      |B X
      |A Z
      |A Y
      |C X
      |A Z
      |A Z
      |C X
      |C Y
      |A Y
      |A Z
      |C X
      |B Y
      |B Y
      |C X
      |A Z
      |C Y
      |A Z
      |A Z
      |A Z
      |A Z
      |B X
      |A Z
      |A X
      |B Z
      |C Z
      |A Z
      |A Z
      |A Y
      |A Z
      |A Y
      |A Z
      |A Z
      |B Y
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |B Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |B Z
      |A Z
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |A Z
      |A Y
      |A Z
      |A Z
      |C Z
      |A Z
      |A Y
      |B Y
      |B Z
      |A Z
      |B Y
      |B Z
      |A Z
      |C X
      |A Z
      |B X
      |A Z
      |A Z
      |A Y
      |A Z
      |B X
      |B Z
      |A Z
      |B Z
      |A Y
      |A Z
      |A Z
      |A Z
      |B Y
      |A Z
      |B Z
      |A Z
      |B Z
      |A Z
      |A Y
      |A Z
      |B Z
      |A Z
      |C X
      |C X
      |A Z
      |A Z
      |B Z
      |A Z
      |A Z
      |C X
      |A Z
      |A X
      |A Z
      |A X
      |C X
      |A Y
      |C Y
      |B Y
      |A Z
      |A Y
      |A X
      |A Z
      |A Y
      |A Z
      |A Z
      |A Z
      |C X
      |B Z
      |A Z
      |B Y
      |A Z
      |A Y
      |A Z
      |A Z
      |B Z
      |C X
      |A Z
      |A Z
      |A Y
      |C X
      |C Y
      |B Z
      |A Z
      |A Z
      |B Z
      |C X
      |A Z
      |A Z
      |A Z
      |C Y
      |A Y
      |A Z
      |B Z
      |B Z
      |A Z
      |B Z
      |A X
      |A Z
      |B Z
      |A Z
      |A Z
      |B Y
      |A Z
      |A Y
      |B Y
      |A Z
      |B Z
      |C X
      |C Y
      |A Z
      |B Z
      |C X
      |A Z
      |B X
      |A Z
      |A Z
      |C X
      |A Z
      |A Y
      |B Z
      |B Y
      |A Z
      |A Y
      |C Y
      |A Z
      |C X
      |B Z
      |A X
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Y
      |A Z
      |A Z
      |C X
      |B Y
      |B Z
      |C X
      |C X
      |C X
      |A Z
      |A Z
      |B Z
      |B Y
      |A Z
      |A Z
      |B Y
      |B Y
      |A Y
      |A Y
      |A X
      |A Y
      |B Y
      |A Y
      |C X
      |A Z
      |A Z
      |A Z
      |A Y
      |B Y
      |A Z
      |C Y
      |B Z
      |C X
      |A Y
      |A Z
      |C Z
      |A Z
      |B Z
      |C X
      |B Z
      |C X
      |B Z
      |B Y
      |B Y
      |C X
      |A Z
      |A Z
      |B Z
      |B Z
      |B Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |C X
      |C X
      |C Y
      |B X
      |C X
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A X
      |A Z
      |C X
      |A Z
      |C Z
      |A Z
      |C X
      |C Z
      |A Z
      |A Z
      |A Y
      |B Z
      |A X
      |A Z
      |A Z
      |A Z
      |C X
      |B Y
      |A Z
      |B Y
      |A Z
      |B Y
      |A Z
      |C X
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |B X
      |B Y
      |C X
      |C X
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |A Y
      |A Z
      |A Y
      |A Z
      |A Z
      |A Z
      |C Y
      |A Z
      |A Z
      |B Z
      |B Y
      |C X
      |A Y
      |A Z
      |B Z
      |A Z
      |A Z
      |A Z
      |B Y
      |C Z
      |A Y
      |C Y
      |A X
      |A X
      |B Z
      |A Y
      |C X
      |C X
      |B Z
      |A Z
      |B Z
      |A Z
      |A Z
      |A Y
      |B Z
      |C Y
      |A Z
      |A Z
      |A Z
      |C X
      |B Y
      |A X
      |A Z
      |C X
      |A Z
      |B Z
      |B Y
      |C X
      |B Y
      |A X
      |A Z
      |A Z
      |A Z
      |A Y
      |A Z
      |A Z
      |A Y
      |A Y
      |A Y
      |A X
      |A Z
      |A Z
      |B Z
      |A Y
      |A Z
      |C X
      |C X
      |A Z
      |A Z
      |B Z
      |B Y
      |A Z
      |A Z
      |C X
      |A Y
      |C X
      |B Z
      |A Z
      |A Z
      |A Z
      |A Y
      |A X
      |C Y
      |C X
      |C X
      |A Z
      |A Y
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |A Z
      |B Z
      |C X
      |C X
      |B Z
      |B Z
      |C X
      |A Z
      |B X
      |A Z
      |B Z
      |A Y
      |A Z
      |A Z
      |C X
      |A Z
      |C Z
      |A Z
      |B Z
      |C X
      |A Z
      |B Y
      |A Y
      |A Z
      |A Z
      |B X
      |C X
      |B Z
      |C X
      |C X
      |C Y
      |B Z
      |C X
      |A Y
      |C X
      |A Z
      |A Z
      |A Z
      |A Y
      |A Z
      |C X
      |C Y
      |A Z
      |A Z
      |B Z
      |A Z
      |B Y
      |A X
      |A X
      |B Z
      |C X
      |B Y
      |A Z
      |C X
      |A Z
      |B Z
      |A Z
      |B Y
      |B Y
      |A X
      |B Y
      |A Z
      |A X
      |A Z
      |C X
      |A Z
      |A Z
      |B Y
      |C Y
      |A X
      |A Z
      |C Y
      |C X
      |A Z
      |A Z
      |A Z
      |A Y
      |A Z
      |C X
      |A Z
      |B Y
      |A Z
      |B Z
      |B Z
      |C X
      |B Y
      |A Z
      |C Y
      |A Y
      |A Z
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |A Z
      |B Y
      |B Z
      |A Z
      |A Z
      |C Y
      |A Z
      |A Z
      |A Z
      |A Z
      |A Y
      |A Z
      |A Z
      |C Y
      |B Y
      |B Z
      |A Z
      |B Z
      |A Z
      |B Z
      |A Z
      |A Z
      |A Y
      |A Z
      |A Z
      |B Z
      |C Y
      |A Z
      |B Z
      |A X
      |C X
      |C X
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |C X
      |A Z
      |B Y
      |A Z
      |C Y
      |A Y
      |B Z
      |C X
      |A Y
      |A Z
      |A Y
      |A Z
      |B Z
      |B Y
      |A Y
      |B Z
      |A Z
      |A Z
      |C Z
      |C Z
      |A Y
      |A Z
      |A Z
      |C X
      |A Z
      |A X
      |C Y
      |C X
      |A Z
      |A Z
      |B Y
      |B Z
      |C Y
      |A Z
      |B Z
      |A Z
      |C Z
      |A Z
      |A Y
      |B Y
      |C X
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |A Y
      |A Z
      |A Z
      |B Z
      |C X
      |C X
      |C X
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |A X
      |A Z
      |C X
      |C X
      |B Z
      |A Z
      |A Z
      |A Z
      |B Y
      |A Z
      |C X
      |A X
      |B Z
      |C X
      |A Z
      |C X
      |A Y
      |A Z
      |A Z
      |A Y
      |A Z
      |A Y
      |A X
      |C X
      |A Z
      |A Y
      |B Y
      |A Z
      |A Y
      |C Z
      |B Y
      |A Z
      |A Z
      |A Y
      |A Z
      |C X
      |A Z
      |B Y
      |B Z
      |B Z
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |B Y
      |B Y
      |A Z
      |A Y
      |C X
      |A Z
      |C X
      |A Z
      |C X
      |C X
      |A Y
      |B Z
      |A Z
      |A Z
      |A Z
      |A Y
      |A X
      |B Y
      |A Z
      |B Z
      |B Y
      |C Y
      |C X
      |C Y
      |A Z
      |B Z
      |A Z
      |A Y
      |B X
      |C Z
      |A X
      |A Z
      |A Z
      |A Z
      |A Y
      |A Y
      |A Z
      |A Y
      |C X
      |A Z
      |A Y
      |C X
      |A Z
      |A Z
      |C Y
      |A Z
      |B Z
      |A Y
      |B Z
      |B X
      |B Z
      |C X
      |A Z
      |A Z
      |C X
      |A Z
      |A Z
      |C X
      |B Z
      |B Z
      |B Z
      |A Z
      |A Z
      |B Y
      |A Z
      |A Z
      |C Z
      |C Z
      |B Z
      |A Z
      |B Y
      |A Z
      |A Z
      |C X
      |B Z
      |A Z
      |A Z
      |A Z
      |C Y
      |B Y
      |C X
      |A Z
      |A Z
      |A Z
      |A Y
      |A Z
      |C X
      |A Z
      |C X
      |A Z
      |A Z
      |C Y
      |C X
      |C X
      |C X
      |C X
      |C X
      |B Z
      |B Z
      |B Y
      |B Y
      |C Z
      |A Z
      |B Z
      |A Z
      |A X
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |C X
      |A Z
      |A Z
      |C X
      |C X
      |B Y
      |A Z
      |C Y
      |A Z
      |A Y
      |C Y
      |B Z
      |B Y
      |C X
      |A Z
      |C Y
      |A Z
      |A Z
      |A Z
      |B Y
      |C X
      |B Z
      |B Z
      |C X
      |B Y
      |B Z
      |A Z
      |C X
      |A Z
      |B Y
      |B Z
      |A Y
      |A Z
      |A Z
      |A Z
      |A Z
      |C Y
      |A Z
      |C Y
      |A Z
      |B Y
      |C X
      |B Z
      |A Z
      |A Z
      |B Z
      |C X
      |B Z
      |A Z
      |A Z
      |A X
      |B Y
      |A Z
      |A Z
      |A Z
      |A Z
      |A Y
      |B Z
      |A Z
      |A Z
      |A Z
      |B Z
      |C Y
      |C X
      |C X
      |B Z
      |A Z
      |B Z
      |C X
      |A Y
      |C X
      |B Y
      |B Z
      |C Y
      |B Y
      |B Y
      |A Y
      |C Y
      |A Z
      |A Z
      |B Z
      |B Z
      |A X
      |A Z
      |A Z
      |B Z
      |A Z
      |B Z
      |A X
      |A Z
      |A Y
      |A Y
      |A Z
      |B Z
      |A Z
      |B Z
      |A Z
      |B Z
      |C X
      |A Z
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |C X
      |B Y
      |A X
      |B Z
      |B Z
      |C Y
      |A Z
      |A Z
      |C X
      |B Y
      |C X
      |A Z
      |A X
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |C X
      |B Z
      |B Z
      |C X
      |A Y
      |C Z
      |A Y
      |A Z
      |C Y
      |A Z
      |A Z
      |A Y
      |A Z
      |C X
      |C Z
      |A Z
      |A Y
      |A Z
      |A X
      |A Y
      |A Z
      |C X
      |B Z
      |A Z
      |B Z
      |C Y
      |C X
      |A Z
      |A Z
      |A X
      |A Z
      |B Z
      |C X
      |A Z
      |B Y
      |A Z
      |A X
      |A Z
      |B Z
      |A Z
      |A Z
      |C X
      |B Z
      |B X
      |A Z
      |A Z
      |C Z
      |A Z
      |A Z
      |A Y
      |B Z
      |A Z
      |C X
      |B Z
      |A Z
      |C Y
      |A Z
      |B Z
      |B Z
      |A Z
      |A Z
      |C X
      |B Z
      |C Y
      |A Y
      |A Y
      |B Z
      |A Z
      |A Z
      |C Z
      |B Z
      |A Y
      |A Y
      |C Z
      |C Z
      |A Z
      |B Z
      |B Y
      |A Z
      |B Y
      |A Z
      |A Z
      |A Z
      |C Y
      |A Y
      |A Z
      |A Z
      |A Z
      |A Z
      |C Z
      |A Z
      |B Y
      |A Z
      |B Z
      |C Y
      |A Z
      |B Y
      |B Z
      |A Z
      |A Y
      |A Z
      |A Z
      |B Z
      |A Z
      |A Z
      |A Z
      |C Z
      |B Z
      |C X
      |B Z
      |A Z
      |A Z
      |A Y
      |A Z
      |A Z
      |A Z
      |A Z
      |C Y
      |A Z
      |A Z
      |A Z
      |A X
      |A Y
      |B Y
      |C X
      |A Z
      |A Z
      |B Y
      |A Z
      |C X
      |A Y
      |A Z
      |A Z
      |C X
      |A X
      |A Z
      |A Z
      |A Z
      |B Y
      |B Z
      |A X
      |A Z
      |C X
      |A Z
      |C X
      |B Z
      |B Y
      |A Z
      |B Y
      |A Y
      |A Z
      |A Y
      |A Z
      |B Y
      |C X
      |B X
      |B Y
      |C X
      |A Z
      |A Z
      |A Y
      |A Z
      |A Y
      |B Z
      |B Y
      |B Y
      |A Y
      |A X
      |A Z
      |C X
      |A Z
      |B X
      |A Z
      |A Z
      |C X
      |B Y
      |B Z
      |B Z
      |B Y
      |B Z
      |A Y
      |C X
      |C Z
      |A Z
      |A Y
      |B Y
      |B Y
      |A Z
      |A Y
      |C Y
      |B Y
      |B Z
      |A Z
      |A Y
      |A Z
      |B Z
      |A Z
      |A Z
      |A Z
      |A Z
      |B Z
      |A Y
      |C X
      |A Z
      |A Z
      |B Y
      |A Z
      |B Y
      |B Z
      |A Z
      |C X
      |A Z
      |C X
      |B Y
      |A Z
      |B Z
      |C X
      |A Z
      |A Z
      |A Z
      |C X
      |C X
      |A Z
      |A Z
      |A Z
      |B Y
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Y
      |B Z
      |C Y
      |A Y
      |C X
      |A Z
      |A Z
      |B Y
      |B X
      |A Z
      |C Z
      |A Z
      |A Y
      |A Z
      |A X
      |A Z
      |A Z
      |A Z
      |C Z
      |C X
      |C X
      |B Y
      |C X
      |C X
      |B Y
      |C Z
      |C X
      |C X
      |A Y
      |A Z
      |C X
      |C Z
      |B Y
      |A Z
      |C X
      |C X
      |A Z
      |A Z
      |A Z
      |A Y
      |A Z
      |A Y
      |B Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |C X
      |C X
      |C X
      |C X
      |A Z
      |A Z
      |B Z
      |A Y
      |A Z
      |A Z
      |B Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |C X
      |C X
      |A Z
      |A Z
      |C X
      |C Z
      |C Z
      |A Z
      |A Z
      |B Y
      |B Z
      |A Z
      |A Z
      |B Y
      |B X
      |C X
      |B Y
      |C X
      |C X
      |A Z
      |C X
      |C X
      |C Y
      |C X
      |C X
      |C X
      |B Z
      |B Z
      |A Z
      |A Z
      |B Z
      |A Z
      |C X
      |A Z
      |B Y
      |A Y
      |A Z
      |C Z
      |B Y
      |B Z
      |B Z
      |A Z
      |C X
      |B Z
      |A X
      |B Y
      |A Y
      |B Y
      |B Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |C X
      |C X
      |B Z
      |A Z
      |B Y
      |A Z
      |B Z
      |A Z
      |B Y
      |A Z
      |A X
      |A Z
      |B Y
      |B Y
      |A Y
      |C X
      |A Z
      |A Z
      |C Y
      |A Z
      |A Z
      |B Z
      |C Z
      |B Z
      |A Z
      |A Z
      |C X
      |C Y
      |B Z
      |A Z
      |C X
      |A Z
      |C X
      |B Y
      |C X
      |B Z
      |A Z
      |A Z
      |C X
      |A Z
      |C X
      |A Y
      |C X
      |C X
      |C Y
      |B Z
      |C X
      |B Z
      |C X
      |A Z
      |A Z
      |C X
      |A Y
      |B Z
      |C X
      |B Z
      |C X
      |A Z
      |C X
      |C X
      |A Z
      |A Z
      |B Y
      |C Z
      |A X
      |A X
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |A Y
      |A Z
      |A Z
      |A Z
      |A Z
      |B Z
      |C X
      |B Y
      |A Z
      |A Z
      |B Y
      |A Z
      |A Z
      |A Z
      |A Z
      |B Y
      |A Z
      |B Z
      |A Y
      |A Y
      |A Y
      |A Z
      |A Y
      |A Z
      |B Z
      |A Z
      |B Y
      |A Z
      |C X
      |A Y
      |C X
      |A Z
      |B Y
      |A Y
      |A Z
      |A Y
      |A Z
      |C Z
      |A Z
      |B Z
      |A Z
      |C X
      |B Z
      |A Z
      |A Z
      |C X
      |B Y
      |A Z
      |C X
      |C Y
      |A Z
      |B Y
      |A Z
      |B X
      |B Y
      |B Y
      |C Z
      |C X
      |A Z
      |C X
      |A Y
      |A Y
      |B Z
      |A Z
      |C X
      |B Z
      |A Z
      |A Z
      |A Y
      |C X
      |C X
      |A Z
      |A X
      |B Z
      |A Z
      |A Z
      |A X
      |A Z
      |C X
      |A Z
      |B Y
      |A Z
      |B X
      |B Y
      |A Y
      |C X
      |A Z
      |A X
      |A X
      |A Y
      |B Y
      |A Y
      |A Z
      |C X
      |B Z
      |A Z
      |A X
      |A X
      |A Z
      |C X
      |B Z
      |C X
      |B Y
      |A Z
      |C X
      |A Y
      |A Z
      |A Z
      |A Z
      |A Z
      |C Z
      |B Z
      |C Z
      |A Z
      |A Z
      |C X
      |B Y
      |A Y
      |B Z
      |C X
      |B Z
      |A Z
      |A Z
      |A Z
      |A X
      |B Y
      |C X
      |B Y
      |B Z
      |B Z
      |A Z
      |A Y
      |C Y
      |A Y
      |A X
      |A Z
      |C X
      |A Z
      |A Z
      |A X
      |A Z
      |A Z
      |C X
      |A Z
      |A X
      |A Z
      |C Y
      |A Z
      |B Z
      |A Y
      |A Z
      |C Z
      |A Y
      |A Y
      |B Z
      |A Y
      |A Y
      |A Y
      |A Z
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A X
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Y
      |B Z
      |A Y
      |A Z
      |C X
      |A X
      |B Z
      |A Z
      |A X
      |C Y
      |B Y
      |B Y
      |B Z
      |C X
      |C X
      |A Z
      |A Z
      |C X
      |C X
      |A Z
      |A X
      |A Z
      |A Z
      |A Z
      |A X
      |C X
      |C X
      |A Z
      |A Y
      |A Z
      |A Z
      |A Z
      |A Z
      |C Z
      |A Z
      |C X
      |B Z
      |C Z
      |C X
      |A Z
      |B Y
      |C X
      |C X
      |B Z
      |C X
      |B Y
      |A Y
      |B X
      |A Z
      |A Z
      |A Z
      |B Z
      |C X
      |A X
      |B Z
      |A X
      |A Z
      |C Z
      |B Z
      |A Z
      |A Z
      |C X
      |C X
      |A X
      |C X
      |C Y
      |A Z
      |A Z
      |A Z
      |B Z
      |C Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |B Z
      |A Z
      |A Z
      |C X
      |B Y
      |A Z
      |B Z
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |B Z
      |C X
      |B Y
      |A Z
      |A Z
      |A Z
      |C Y
      |A Z
      |A Z
      |C X
      |C X
      |A Y
      |B Z
      |A Z
      |C Y
      |C X
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |A Z
      |B Z
      |B Z
      |A Z
      |A Z
      |A Z
      |C X
      |C X
      |A Z
      |B Z
      |A X
      |B Y
      |C X
      |C X
      |C X
      |A Z
      |C X
      |A Z
      |A Z
      |B Y
      |A Y
      |B Z
      |B Z
      |A Z
      |A Z
      |C Z
      |A Y
      |A Y
      |A Z
      |A Z
      |C X
      |C Z
      |A Z
      |A Z
      |A Z
      |A Z
      |C Y
      |B Y
      |B Y
      |C Y
      |A Z
      |C X
      |B Z
      |A X
      |A Y
      |B Z
      |A Z
      |A Z
      |A Z
      |A X
      |A Z
      |A Z
      |A Z
      |A Y
      |C Z
      |A Y
      |C X
      |A Y
      |A X
      |C X
      |A Z
      |B Y
      |A Y
      |B Y
      |A Z
      |C X
      |C Z
      |A Z
      |C Z
      |A Y
      |A Z
      |A Z
      |B Z
      |B X
      |B Z
      |B Z
      |B Y
      |C Z
      |C X
      |A X
      |C Z
      |B Z
      |A Z
      |A Z
      |B Z
      |A Y
      |A Z
      |B Y
      |C X
      |A Z
      |B Z
      |C X
      |A X
      |B Z
      |A Z
      |A Z
      |C X
      |C X
      |C X
      |B Z
      |C Z
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |A Z
      |C Z
      |C X
      |A X
      |B Y
      |A Z
      |C X
      |C Z
      |A Z
      |C X
      |B Z
      |B Y
      |A Z
      |C X
      |A Z
      |B Z
      |C Z
      |A Z
      |A Z
      |A X
      |B Y
      |C X
      |A Z
      |C X
      |A Z
      |A X
      |C X
      |A Z
      |C X
      |A Y
      |A Z
      |C X
      |C Z
      |C Y
      |B Z
      |B Z
      |A Y
      |B Z
      |A Z
      |A Z
      |A Y
      |A Z
      |C X
      |C X
      |A Z
      |A Z
      |C X
      |B Z
      |A Z
      |B Y
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |A Y
      |B Y
      |A Z
      |C X
      |A Z
      |A X
      |A Z
      |A Z
      |C X
      |C Y
      |A Z
      |C X
      |A Z
      |A Y
      |C X
      |B Z
      |C Z
      |B Y
      |A Z
      |A Z
      |B Y
      |A Z
      |B Y
      |A Z
      |A Z
      |A Z
      |C X
      |A Y
      |C X
      |A Z
      |C Y
      |A Z
      |A Z
      |A Z
      |B Y
      |B Z
      |B Z
      |C Z
      |A X
      |A Z
      |A Z
      |C X
      |A Z
      |C X
      |A Z
      |A X
      |C Z
      |C X
      |A Z
      |A Z
      |A Z
      |A Z
      |B Z
      |A Z
      |A Z
      |C Y
      |A Y
      |A Z
      |C Y
      |C Y
      |A Z
      |B Z
      |A Z
      |B Z
      |A Z
      |B Z
      |C Y
      |C X
      |C X
      |A Y
      |A Z
      |B Z
      |C X
      |A X
      |A Z
      |A Z
      |A Y
      |A Z
      |A Z
      |A Z
      |C X
      |C X
      |A Y
      |A Z
      |A Z
      |C X
      |B Z
      |B Y
      |A Y
      |A Z
      |B Z
      |A Z
      |A Z
      |A X
      |A Z
      |A Z
      |B Y
      |A Z
      |A Y
      |B Z
      |C X
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |B Z
      |B Z
      |A Z
      |A Z
      |A Z
      |A Z
      |B Z
      |A Z
      |A Z
      |A Z
      |A Z
      |C X
      |C X
      |A Z
      |B X
      |A Z
      |B Y
      |C Z
      |A Z
      |A Z
      |A Z
      |C Z
      |A Z
      |B Y
      |A X
      |A Z
      |C Z
      |A Z
      |C Z
      |A Z
      |C X
      |A Y
      |C X
      |C Z
      |A Y
      |A Z
      |A Z
      |C X
      |C X
      |A Z
      |A Z
      |A Z
      |B Z
      |B Z
      |A Y
      |B Y
      |C Z
      |B Z
      |B X
      |A Z
      |A Z
      |A Z
      |C X
      |B Y
      |A Z
      |B Z
      |C X
      |A Z
      |A Z
      |C X
      |C X
      |A Z
      |A Z
      |A Z
      |A Z
      |B Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |B Z
      |A Z
      |A Z
      |B Z
      |A Y
      |A Z
      |B Y
      |A Z
      |A Z
      |A Z
      |B Z
      |A Z
      |A Z
      |C X
      |B Y
      |C X
      |A Y
      |C X
      |A Z
      |A Z
      |A Z
      |C X
      |B Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Y
      |C X
      |B Y
      |C X
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |B Y
      |A Z
      |C X
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |C X
      |A Y
      |B Z
      |A Z
      |C X
      |A Z
      |C X
      |B Z
      |B Z
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |B Z
      |B Y
      |C X
      |C X
      |C X
      |B Z
      |A X
      |A Y
      |A Z
      |A Z
      |A Z
      |A Y
      |A Z
      |A Z
      |A Y
      |A X
      |A Y
      |C X
      |A Y
      |C X
      |B Z
      |B Y
      |C X
      |B Y
      |B Y
      |C X
      |A Z
      |A Z
      |B Z
      |C X
      |A Z
      |A Z
      |B Y
      |A Z
      |B Y
      |A Z
      |A X
      |C Y
      |C X
      |B Z
      |C X
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |C X
      |A X
      |A Y
      |A Z
      |A Z
      |C X
      |A Y
      |A Z
      |B X
      |A Y
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |C X
      |A Z
      |C Y
      |A Z
      |A Z
      |B Z
      |A Y
      |A Z
      |B Y
      |B Y
      |A Z
      |C X
      |A Z
      |C X
      |C Z
      |A Z
      |B Z
      |B Z
      |A Z
      |A Z
      |C X
      |C X
      |B Y
      |A Z
      |A Z
      |B Z
      |C Z
      |B Y
      |B Y
      |C X
      |A Z
      |A Z
      |B Y
      |A Z
      |C X
      |C Z
      |A Z
      |B Z
      |C X
      |A Y
      |C X
      |A Z
      |B Y
      |B Y
      |A Y
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Y
      |A Z
      |A Y
      |A Z
      |B Y
      |B Y
      |B Y
      |C X
      |A Z
      |A Z
      |A Y
      |A Z
      |A Z
      |C X
      |A Y
      |C X
      |A Z
      |A Z
      |A Z
      |B Y
      |A Z
      |A Z
      |B Z
      |C X
      |A Z
      |B Y
      |B Y
      |C X
      |C Z
      |B Y
      |A Z
      |A Z
      |A Z
      |C X
      |A Z
      |B Z
      |C Z
      |C X
      |B Y
      |A Z
      |A Z
      |A Y
      |A X
      |A Z
      |A Z
      |A Z
      |A Z
      |A Z
      |A Y
      |B Y
      |A Z
      |B Z
      |B Z
      |A Z
      |C Y
      |A X
      |A Z
      |A Z
      |C X
      |C X
      |C Y
      |B Y
      |B Y
      |B X
      |A Z
      |A Z
      |B Z
      |A Z""".stripMargin
}

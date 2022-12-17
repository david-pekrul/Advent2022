package helpers

import scala.io.Source

object Helpers {
  def readFile(filePath: String) = {
    Source.fromFile(filePath).getLines().toSeq
  }

  def getLetterIndex(input: Char) = {
    val intValue = input.asInstanceOf[Int]
    if (input.isLower) {
      intValue - 96
    } else {
      intValue - 38
    }
  }
}

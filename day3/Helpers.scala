package helpers

import scala.io.Source

object Helpers {
  def readFile(filePath: String) = {
    Source.fromFile(filePath).getLines().toSeq
  }
}

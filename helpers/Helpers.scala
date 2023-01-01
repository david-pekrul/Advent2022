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

  def infiniteStream[A](seed: Seq[A]): LazyList[A] = {
    val x = seed.to(LazyList)

    def xs: LazyList[A] = x #::: infiniteStream(seed)

    xs
  }

  def infiniteIndexedStream[A](seed: Seq[A]): LazyList[(A, Int)] = {
    val indexed = seed.zipWithIndex.to(LazyList)

    def xs: LazyList[(A, Int)] = indexed #::: infiniteIndexedStream(seed)

    xs
  }
}

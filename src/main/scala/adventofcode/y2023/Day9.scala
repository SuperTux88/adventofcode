package adventofcode.y2023

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day9 extends Year2023 {
  override val day = 9

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines().takeWhile(_.nonEmpty).map(
      _.split(" ").map(_.toInt).toList
    ).toList

    val diffChains = lines.map(getDifferencesChain)

    val newNumbers = diffChains.map(findNextNumber(_, _.last + _))
    printDayPart(1, newNumbers.sum, "Sum of all next numbers: %s")

    val prevNumbers = diffChains.map(findNextNumber(_, _.head - _))
    printDayPart(2, prevNumbers.sum, "Sum of all previous numbers: %s")
  }

  private def getDifferencesChain(numbers: List[Int]): List[List[Int]] = {
    @tailrec
    def reduce(chain: List[List[Int]]): List[List[Int]] = {
      val diffs = chain.head.sliding(2).map { x =>
        (x: @unchecked) match {
          case Seq(a, b) => b - a
        }
      }.toList
      if (diffs.forall(_ == 0)) chain
      else reduce(diffs :: chain)
    }

    reduce(List(numbers))
  }

  private def findNextNumber(diffChains: List[List[Int]], operation: (List[Int], Int) => Int): Int = {
    val newNumbers = diffChains.foldLeft(List.empty[Int]) { (newNumbers, list) =>
      val nextDiff = newNumbers.headOption.getOrElse(0)
      operation(list, nextDiff) :: newNumbers
    }

    newNumbers.head
  }
}

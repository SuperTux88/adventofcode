package adventofcode.y2020

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day9 extends Year2020 {
  override val day = 9

  override def runDay(input: BufferedSource): Unit = {
    val numbers = input.getLines().map(_.toLong).toVector

    val firstWeaknessNumber = numbers.sliding(26).find { window =>
      !window.init.combinations(2).exists(_.sum == window.last)
    }.get.last

    printDayPart(1, firstWeaknessNumber, "first number with weakness: %s")
    printDayPart(2, findContiguousRange(numbers, 2, firstWeaknessNumber), "encryption weakness: %s")
  }

  @tailrec
  private def findContiguousRange(numbers: Seq[Long], n: Int, searchedSum: Long): Long =
    numbers.sliding(n).find(_.sum == searchedSum) match {
      case None => findContiguousRange(numbers, n + 1, searchedSum)
      case Some(range) => range.min + range.max
    }
}

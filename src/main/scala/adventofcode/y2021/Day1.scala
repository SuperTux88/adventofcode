package adventofcode.y2021

import scala.io.BufferedSource

object Day1 extends Year2021 {
  override val day = 1

  override def runDay(input: BufferedSource): Unit = {
    val depths = input.getLines().takeWhile(_.nonEmpty).map(_.toInt).toSeq

    printDayPart(1, countIncreasing(depths), "depth measurement increases %s times")
    printDayPart(2, countIncreasing(depths, 3), "depth sliding window increases %s times")
  }

  private def countIncreasing(numbers: Seq[Int], groupSize: Int = 1) =
    numbers.sliding(groupSize + 1).count(n => n.head < n.last)
}

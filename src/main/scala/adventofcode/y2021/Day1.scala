package adventofcode.y2021

import scala.io.BufferedSource

object Day1 extends Year2021 {
  override val day = 1

  override def runDay(input: BufferedSource): Unit = {
    val depths = input.getLines().takeWhile(_.nonEmpty).map(_.toInt).toSeq

    printDayPart(1, countIncreasing(depths), "depth measurement increases %s times")

    val depthGroups = depths.sliding(3).map(_.sum).toSeq
    printDayPart(2, countIncreasing(depthGroups), "depth sliding window increases %s times")
  }

  private def countIncreasing(numbers: Seq[Int]) = numbers.sliding(2).count { case Seq(a, b) => a < b }
}

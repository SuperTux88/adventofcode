package adventofcode.y2022

import scala.io.BufferedSource

object Day3 extends Year2022 {
  override val day = 3

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines().toSeq

    val compartments = lines.map(line => line.splitAt(line.length / 2))
    val commonItems = compartments.map { case (first, second) => (first intersect second).head }

    printDayPart(1, commonItems.map(getPriority).sum, "Sum of priorities with compartments: %s")

    val groups = lines.grouped(3)
    val commonItems2 = groups.map { case Seq(first, second, third) => (first intersect second intersect third).head }

    printDayPart(2, commonItems2.map(getPriority).sum, "Sum of priorities with groups: %s")
  }

  private def getPriority(item: Char) = if (item >= 'a' && item <= 'z') item - 'a' + 1 else item - 'A' + 27
}

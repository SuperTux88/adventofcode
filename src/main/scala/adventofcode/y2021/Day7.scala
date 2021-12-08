package adventofcode.y2021

import adventofcode.common.NumberHelper.increasingSum

object Day7 extends Year2021 {
  override val day = 7

  override def runDay(input: String): Unit = {
    val crabs = input.split(",").map(_.toInt).toSeq

    val median = crabs.sorted.drop(crabs.length / 2).head
    printDayPart(1, crabs.map(c => math.abs(c - median)).sum, "fuel spent to align positions: %s")

    val mean = crabs.sum / crabs.size
    val minFuelCost = Seq(0, 1).map(d => crabs.map(c => increasingSum(math.abs(c -  (mean + d)))).sum).min
    printDayPart(2, minFuelCost, "fuel spent to align positions with increasing costs: %s")
  }
}

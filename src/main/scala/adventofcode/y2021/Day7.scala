package adventofcode.y2021

import adventofcode.common.NumberHelper.increasingSum

import scala.annotation.tailrec

object Day7 extends Year2021 {
  override val day = 7

  override def runDay(input: String): Unit = {
    val crabs = input.split(",").map(_.toInt).toSeq

    val median = crabs.sorted.drop(crabs.length / 2).head
    printDayPart(1, crabs.map(c => math.abs(c - median)).sum, "fuel spent to align positions: %s")

    val mean = crabs.sum / crabs.size
    val minFuelCost = getInitialDirection(crabs, mean) match {
      case (meanFuelCost, 0) => meanFuelCost
      case (meanFuelCost, direction) => searchBestPosition(crabs, mean, meanFuelCost, direction)
    }
    printDayPart(2, minFuelCost, "fuel spent to align positions with increasing costs: %s")
  }

  private def increasingFuelCostToTarget(crabs: Seq[Int], target: Int) =
    crabs.map(c => increasingSum(math.abs(c - target))).sum

  private def getInitialDirection(crabs: Seq[Int], target: Int) = {
    val costs = Seq(-1, 0, +1).map(d => increasingFuelCostToTarget(crabs, target + d))
    (costs(1), costs.indexOf(costs.min) - 1)
  }

  @tailrec
  private def searchBestPosition(crabs: Seq[Int], target: Int, fuelCost: Int, direction: Int): Int = {
    val newFuelCost = increasingFuelCostToTarget(crabs, target + direction)
    if newFuelCost > fuelCost then fuelCost else searchBestPosition(crabs, target + direction, newFuelCost, direction)
  }
}

package adventofcode.y2021

import adventofcode.common.IterableImplicits

object Day6 extends Year2021 {
  override val day = 6

  override def runDay(input: String): Unit = {
    val perDaysRemaining = input.split(",").toIterable.groupCount(_.toInt)

    val after80Days = (0 until 80).foldLeft(perDaysRemaining)((currentPerDays, _) => calculateDay(currentPerDays))
    printDayPart(1, after80Days.values.sum, "there are lanternfish %s after 80 days")

    val after256Days = (80 until 256).foldLeft(after80Days)((currentPerDays, _) => calculateDay(currentPerDays))
    printDayPart(2, after256Days.values.sum, "there are lanternfish %s after 256 days")
  }

  private def calculateDay(perDaysRemaining: Map[Int, Long]) = {
    perDaysRemaining.foldLeft(Map[Int, Long]().withDefaultValue(0L)) { (state, daysRemaining) =>
      daysRemaining match {
        case (0, count) => state.updated(6, state(6) + count).updated(8, count)
        case (remaining, count) => state.updated(remaining - 1, state(remaining - 1) + count)
      }
    }
  }
}

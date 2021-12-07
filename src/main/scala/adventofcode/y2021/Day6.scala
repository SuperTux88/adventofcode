package adventofcode.y2021

object Day6 extends Year2021 {
  override val day = 6

  override def runDay(input: String): Unit = {
    val fishes = input.split(",").map(_.toInt).toSeq
    val perDaysRemaining = Vector.tabulate(9)(days => fishes.count(_ == days).toLong)

    val after80Days = (0 until 80).foldLeft(perDaysRemaining)((currentPerDays, _) => calculateDay(currentPerDays))
    printDayPart(1, after80Days.sum, "there are lanternfish %s after 80 days")

    val after256Days = (80 until 256).foldLeft(after80Days)((currentPerDays, _) => calculateDay(currentPerDays))
    printDayPart(2, after256Days.sum, "there are lanternfish %s after 256 days")
  }

  private def calculateDay(perDaysRemaining: Vector[Long]) = {
    val zeroDays +: nonZeroDays = perDaysRemaining
    nonZeroDays.updated(6, nonZeroDays(6) + zeroDays) :+ zeroDays
  }
}

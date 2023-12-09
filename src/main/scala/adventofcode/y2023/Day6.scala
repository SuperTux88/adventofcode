package adventofcode.y2023

import scala.io.BufferedSource

object Day6 extends Year2023 {
  override val day = 6

  override def runDay(input: BufferedSource): Unit = {
    val numbers = input.getLines().take(2).map {
      _.split("\\s+").drop(1).toSeq
    }.toSeq

    val races = numbers.map(_.map(_.toLong)).transpose
    val wins = races.map { case Seq(time, distance) => calculateWins(time, distance) }
    printDayPart(1, wins.product, "Product of numbers of ways to beat the record: %s")

    val Seq(realTime, realDistance) = numbers.map(_.mkString.toLong)
    printDayPart(2, calculateWins(realTime, realDistance), "Number of ways to beat the record with real race: %s")
  }

  private def calculateWins(time: Long, distance: Long): Int = {
    // Use quadratic formula to find the time at which it starts to win against the distance
    val start = (time - math.sqrt(math.pow(time.toDouble, 2) - 4 * distance)) / 2
    // There is an equal ammount of seconds also at the end, where it doesn't win. So just removing these twice works.
    (time - 2 * (start + 1).floor + 1).toInt
  }
}

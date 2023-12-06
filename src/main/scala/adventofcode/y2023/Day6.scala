package adventofcode.y2023

import scala.io.BufferedSource

object Day6 extends Year2023 {
  override val day = 6

  override def runDay(input: BufferedSource): Unit = {
    val numbers = input.getLines().take(2).map {
      _.split("\\s+").drop(1).toSeq
    }.toSeq

    val races = numbers.map(_.map(_.toLong)).transpose
    val wins = races.map { case Seq(time, distance) => countWins(time, distance) }
    printDayPart(1, wins.product, "Product of numbers of ways to beat the record: %s")

    val Seq(realTime, realDistance) = numbers.map(_.mkString.toLong)
    printDayPart(2, countWins(realTime, realDistance), "Number of ways to beat the record with real race: %s")
  }

  private def race(pushTime: Long, raceTime: Long): Long = pushTime * (raceTime - pushTime)
  private def countWins(time: Long, distance: Long): Int =
    // The win count is mirrored, so we can just only the first half and double it
    (1L to time / 2).count(race(_, time) > distance) * 2 - ((time - 1) % 2).toInt
}

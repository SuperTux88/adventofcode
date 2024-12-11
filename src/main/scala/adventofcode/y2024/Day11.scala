package adventofcode.y2024

import adventofcode.common.MapImplicits.IntegralMapImplicits

object Day11 extends Year2024 {
  override val day = 11

  override def runDay(input: String): Unit = {
    val stones = input.split(" ").map(_.toLong).groupBy(identity).view.mapValues(_.length.toLong).toMap

    val result25 = (1 to 25).foldLeft(stones)((stones, _) => blink(stones))
    printDayPart(1, result25.values.sum, "Number of stones after 25 blinks: %s")

    val result75 = (26 to 75).foldLeft(result25)((stones, _) => blink(stones))
    printDayPart(2, result75.values.sum, "Number of stones after 75 blinks: %s")
  }

  private def blink(stones: Map[Long, Long]): Map[Long, Long] =
    stones.foldLeft(Map.empty[Long, Long]) {
      case (newStones, (stone, count)) =>
        if (stone == 0) {
          newStones.changeBy(1, count)
        } else {
          val length = Math.log10(stone.toDouble).toInt + 1
          if (length % 2 == 0) {
            val (a, b) = stone.toString.splitAt(length / 2)
            newStones.changeBy(a.toLong, count).changeBy(b.toLong, count)
          } else {
            newStones.changeBy(stone * 2024, count)
          }
        }
    }
}

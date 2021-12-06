package adventofcode.y2020

import adventofcode.common.IterableImplicits

import scala.io.BufferedSource

object Day10 extends Year2020 {
  override val day = 10

  override def runDay(input: BufferedSource): Unit = {
    val adapters = input.getLines().map(_.toInt).toVector.sorted
    val joltages = 0 +: adapters :+ adapters.last + 3

    val steps = joltages.sliding(2).map(group => group(1) - group(0)).toSeq.groupCount(identity)
    printDayPart(1, steps(1) * steps(3))

    val ways = joltages.zip(1 to joltages.size).reverse.tail
      .foldLeft(List(1L)) { (ways, current) =>
        val (joltage, previousIndex) = current
        val waysFromThisJoltageToDevice = (0 until Math.min(3, joltages.size - previousIndex))
          .takeWhile(diff => joltages(previousIndex + diff) - joltage <= 3)
          .map(ways(_)).sum
        waysFromThisJoltageToDevice :: ways
      }

    printDayPart(2, ways.head, "number of distinct ways to arrange the adapters: %s")
  }
}

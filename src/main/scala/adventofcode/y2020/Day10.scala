package adventofcode.y2020

object Day10 extends Year2020 {
  override val day = 10

  private val adapters = input.getLines().map(_.toInt).toVector.sorted
  private val joltages = 0 +: adapters :+ adapters.last + 3

  private val steps = joltages.sliding(2).map(group => group(1) - group(0))
    .toList.groupBy(identity).view.mapValues(_.size)

  printDayPart(1, steps(1) * steps(3))

  private val ways = joltages.zip(1 to joltages.size).reverse.tail
    .foldLeft(List(1L)) { (ways, current) =>
      val (joltage, previousIndex) = current
      val waysFromThisJoltageToDevice = (0 until Math.min(3, joltages.size - previousIndex))
        .takeWhile(diff => joltages(previousIndex + diff) - joltage <= 3)
        .map(ways(_)).sum
      waysFromThisJoltageToDevice :: ways
    }

  printDayPart(2, ways.head, "number of distinct ways to arrange the adapters: %s")
}

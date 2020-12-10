package adventofcode.y2020

object Day10 extends Year2020 {
  override val day = 10

  private val adapters = input.getLines().map(_.toInt).toVector.sorted
  private val joltages = 0 +: adapters :+ adapters.last + 3

  private val steps = joltages.sliding(2).map(group => group(1) - group(0))
    .toList.groupBy(identity).view.mapValues(_.size)

  printDayPart(1, steps(1) * steps(3))

  private val ways = joltages.zipWithIndex.tail.foldLeft(Vector(1L)) { (ways, current) =>
    val (joltage, index) = current
    val stepsToHere = (1 to 3).map(index - _)
      .takeWhile(previous => previous >= 0 && joltage - joltages(previous) <= 3)
      .foldLeft(0L)((step, previous) => step + ways(previous))
    ways :+ stepsToHere
  }

  printDayPart(2, ways.last, "number of distinct ways to arrange the adapters: %s")
}

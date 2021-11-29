package adventofcode.y2020

object Day6 extends Year2020 {
  override val day = 6

  override def runDay(input: String): Unit = {
    val groups = input.split("\n\n").map(_.split('\n'))

    printDayPart(1, groups.map(_.mkString.toSet.size).sum, "sum of answers per group: %s")

    val intersections = groups.map(_.reduceLeft(_.intersect(_)))

    printDayPart(2, intersections.map(_.length).sum, "sum of answers where everyone in group answered yes: %s")
  }
}

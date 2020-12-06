package adventofcode.y2020

object Day6 extends Year2020 {
  override val day = 6

  private val groups = inputString.split("\n\n").map(_.split('\n'))

  printDayPart(1, groups.map(_.mkString.toSet.size).sum, "sum of answers per group: %s")

  private val intersections = groups.map { group =>
    group.tail.foldLeft(group.head)(_.intersect(_))
  }

  printDayPart(2, intersections.map(_.length).sum, "sum of answers where everyone in group answered yes: %s")
}

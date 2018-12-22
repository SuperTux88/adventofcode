package adventofcode.y2015

import adventofcode.common.Pos

object Day3 extends Year2015 {
  override val day: Int = 3

  printDayPart(1, part1, "total %s visits in unique houses")
  printDayPart(2, part2, "total %s visits in unique houses")

  private def part1: Int = {
    input.foldLeft(List(Pos(0, 0))) { (positions, dir) =>
      positions.head.move(dir) :: positions
    }.distinct.size
  }

  private def part2: Int = {
    var santaPos, roboSantaPos = Pos(0, 0)

    val houses = Pos(0, 0) :: input.zipWithIndex.map {
      case (char, index) =>
        if (index % 2 == 0) {
          santaPos = santaPos.move(char)
          santaPos
        } else {
          roboSantaPos = roboSantaPos.move(char)
          roboSantaPos
        }
    }.toList

    houses.distinct.size
  }
}

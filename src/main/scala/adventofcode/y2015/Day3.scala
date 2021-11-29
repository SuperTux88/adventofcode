package adventofcode.y2015

import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day3 extends Year2015 {
  override val day: Int = 3

  override def runDay(input: BufferedSource): Unit = {
    val moves = input.toSeq

    printDayPart(1, part1(moves), "total %s visits in unique houses")
    printDayPart(2, part2(moves), "total %s visits in unique houses")
  }

  private def part1(moves: Seq[Char]): Int = {
    moves.foldLeft(List(Pos.zero)) { (positions, dir) =>
      positions.head.move(dir) :: positions
    }.distinct.size
  }

  private def part2(moves: Seq[Char]): Int = {
    var santaPos, roboSantaPos = Pos.zero

    val houses = Pos.zero :: moves.zipWithIndex.map {
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

package adventofcode.y2021

import adventofcode.Logging
import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day5 extends Year2021 {
  override val day = 5

  private val VentRE = """(\d+),(\d+) -> (\d+),(\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val (straightVents, diagonalVents) = input.getLines().takeWhile(_.nonEmpty).map {
      case VentRE(x1, y1, x2, y2) => (Pos(x1.toInt, y1.toInt), Pos(x2.toInt, y2.toInt))
    }.partition(v => v._1.x == v._2.x || v._1.y == v._2.y)

    val grid = straightVents.foldLeft(Map[Pos, Int]().withDefaultValue(0))((map, vent) => markVent(map, vent._1, vent._2))
    printDayPart(1, grid.values.count(_ >= 2), "dangerous areas: %s")

    val gridWithDiagonals = diagonalVents.foldLeft(grid)((map, vent) => markVent(map, vent._1, vent._2))
    printDayPart(2, gridWithDiagonals.values.count(_ >= 2), "dangerous areas with diagonal vents: %s")
  }

  private def rangeWithReverse(from: Int, to: Int) = if from < to then from to to else from to to by -1

  private def posRange(from: Pos, to: Pos) =
    if (from.x == to.x)
      rangeWithReverse(from.y, to.y).map(y => Pos(from.x, y))
    else if (from.y == to.y)
      rangeWithReverse(from.x, to.x).map(x => Pos(x, from.y))
    else
      rangeWithReverse(from.x, to.x).zip(rangeWithReverse(from.y, to.y)).map(Pos(_, _))

  private def markVent(map: Map[Pos, Int], from: Pos, to: Pos) =
    posRange(from, to).foldLeft(map)((m, p) => m.updated(p, m(p) + 1))
}

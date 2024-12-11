package adventofcode.y2021

import adventofcode.common.MapImplicits.IntegralMapImplicits
import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day5 extends Year2021 {
  override val day = 5

  private val VentRE = """(\d+),(\d+) -> (\d+),(\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val (straightVents, diagonalVents) = input.getLines().takeWhile(_.nonEmpty).map {
      case VentRE(x1, y1, x2, y2) => (Pos(x1.toInt, y1.toInt), Pos(x2.toInt, y2.toInt))
    }.partition(v => v._1.x == v._2.x || v._1.y == v._2.y)

    val grid = straightVents.foldLeft(Map[Pos, Int]())((map, vent) => markVent(map, vent._1, vent._2))
    printDayPart(1, grid.values.count(_ >= 2), "dangerous areas: %s")

    val gridWithDiagonals = diagonalVents.foldLeft(grid)((map, vent) => markVent(map, vent._1, vent._2))
    printDayPart(2, gridWithDiagonals.values.count(_ >= 2), "dangerous areas with diagonal vents: %s")
  }

  private def markVent(map: Map[Pos, Int], from: Pos, to: Pos) =
    from.lineTo(to).foldLeft(map)(_.changeBy(_, 1))
}

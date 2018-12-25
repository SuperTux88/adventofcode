package adventofcode.y2018

import adventofcode.common.Pos4D

object Day25 extends Year2018 {
  override val day = 25

  private val PointRE = """ *(-?\d+), *(-?\d+), *(-?\d+), *(-?\d+)""".r

  private val constellations = input.getLines.foldLeft(List[List[Pos4D]]()) {
    case (const, PointRE(d1, d2, d3, d4)) =>
      val pos = Pos4D(d1.toInt, d2.toInt, d3.toInt, d4.toInt)
      val (matching, other) = const.partition(_.exists(_.distance(pos) <= 3))
      (pos :: matching.flatten) :: other
  }

  printDayPart(1, constellations.size)
}

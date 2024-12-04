package adventofcode.y2024

import adventofcode.common.pos.{Direction, Pos}

import scala.io.BufferedSource

object Day4 extends Year2024 {
  override val day = 4

  private val XMAS = "XMAS".toCharArray
  private val POS_DIRECTIONS = Direction.directionsWithDiagonals.map(d => Pos(d._1, d._2))

  override def runDay(input: BufferedSource): Unit = {
    val map = Pos.parseMap(input.getLines(), char => char).withDefaultValue('.')

    val countPerStartPos = map.keys.toSeq.map(pos => POS_DIRECTIONS.count(isXMAS(map, pos, _)))
    printDayPart(1, countPerStartPos.sum, "Count of XMAS: %s")

    printDayPart(2, map.keys.count(isX_MAS(map, _)), "Count of X-MAS: %s")
  }

  private def isXMAS(map: Map[Pos, Char], pos: Pos, dir: Pos): Boolean =
    XMAS.indices.forall { i => map(pos + dir * i) == XMAS(i) }

  private def isX_MAS(map: Map[Pos, Char], pos: Pos): Boolean = {
    if (map(pos) != 'A') return false

    val x = pos.diagonals.map(map)
    x.count(_ == 'M') == 2 && x.count(_ == 'S') == 2 && x.head != x(2) && x(1) != x(3)
  }
}

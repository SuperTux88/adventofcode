package adventofcode.y2021

import adventofcode.common.pos.Pos
import adventofcode.common.search.Dijkstra

import scala.io.BufferedSource

object Day15 extends Year2021 {
  override val day = 15

  override def runDay(input: BufferedSource): Unit = {
    val map = Pos.parseMap(input.getLines(), _.asDigit)
    val end = map.keys.maxBy(identity)

    printDayPart(1, getLowestRisk(Pos.zero, end, getRiskValue(map)), "lowest risk to the bottom right: %s")

    val size = end + (1, 1)
    val endBig = size * 5 - Pos(1, 1)
    printDayPart(2, getLowestRisk(Pos.zero, endBig, getRiskValueForFullMap(map, size)),
      "lowest risk to the bottom right with full map: %s")
  }

  private def getLowestRisk(start: Pos, end: Pos, riskValue: Pos => Option[(Int, Pos)]): Int =
    Dijkstra(start, _ == end, _.directions.flatMap(riskValue))._1

  private def getRiskValue(map: Map[Pos, Int])(pos: Pos): Option[(Int, Pos)] =
    map.get(pos) match {
      case None => None
      case Some(value) => Some(value, pos)
    }

  private def getRiskValueForFullMap(map: Map[Pos, Int], size: Pos)(pos: Pos): Option[(Int, Pos)] = {
    if (pos.x >= size.x * 5 || pos.y >= size.y * 5)
      return None
    map.get(Pos(pos.x % size.x, pos.y % size.y)) match {
      case None => None
      case Some(value) =>
        val xDiff = pos.x / size.x
        val yDiff = pos.y / size.y
        val newRiskValue = value + xDiff + yDiff
        Some(if newRiskValue > 9 then newRiskValue % 9 else newRiskValue, pos)
    }
  }
}

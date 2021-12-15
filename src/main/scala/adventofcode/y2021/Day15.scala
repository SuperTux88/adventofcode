package adventofcode.y2021

import adventofcode.common.pos.Pos
import adventofcode.common.search.Dijkstra

import scala.io.BufferedSource

object Day15 extends Year2021 {
  override val day = 15

  override def runDay(input: BufferedSource): Unit = {
    val map = Pos.parseMap(input.getLines(), _.asDigit);
    val end = map.keys.maxBy(identity)
    val size = end + (1, 1)

    printDayPart(1, getLowestRisk(Pos.zero, end, map), "lowest risk to the bottom right: %s")

    val endBig = size * 5 - Pos(1, 1)

    val fullMap = (0 to endBig.x).flatMap { x =>
      (0 to endBig.y).map { y =>
        val pos = Pos(x, y)
        pos -> getPosValue(map, pos, size)
      }
    }.toMap

    printDayPart(2, getLowestRisk(Pos.zero, endBig, fullMap), "lowest risk to the bottom right with full map: %s")
  }

  private def getLowestRisk(start: Pos, end: Pos, map: Map[Pos, Int]): Int = {
    Dijkstra(
      start,
      _ == end,
      _.directions.filter(map.contains(_)).map((next) => (map(next), next))
    )._1
  }

  private def getPosValue(map: Map[Pos, Int], pos: Pos, size: Pos): Int = {
    val origPos = Pos(pos.x % size.x, pos.y % size.y)
    val xDiff = pos.x / size.x
    val yDiff = pos.y / size.y

    val newPosValue = map(origPos) + xDiff + yDiff
    if newPosValue > 9 then newPosValue % 9 else newPosValue
  }
}

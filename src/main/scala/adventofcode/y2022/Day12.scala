package adventofcode.y2022

import adventofcode.common.pos.Pos
import adventofcode.common.search.Dijkstra

import scala.io.BufferedSource

object Day12 extends Year2022 {
  override val day = 12

  override def runDay(input: BufferedSource): Unit = {
    val map = Pos.parseMap(input.getLines(), identity)
    val (start, end) = (map.find(_._2 == 'S').get._1, map.find(_._2 == 'E').get._1)

    val heightmap = map.view.mapValues {
      case 'S' => 0
      case 'E' => 25
      case c => c - 'a'
    }.toMap

    printDayPart(1, getFewestSteps(end, _ == start, heightmap), "Fewest step from start: %s")
    printDayPart(2, getFewestSteps(end, heightmap(_) == 0, heightmap), "Fewest steps from any possible start: %s")
  }

  // going reverse to find the closest possible start
  private def getFewestSteps(end: Pos, isStart: Pos => Boolean, heightmap: Map[Pos, Int]): Int =
    Dijkstra(end, isStart, getNeighbors(heightmap))._1

  private def getNeighbors(heightmap: Map[Pos, Int])(pos: Pos) =
    pos.directions.filter(n => heightmap.contains(n) && heightmap(n) - heightmap(pos) >= -1).map(n => (1, n))
}

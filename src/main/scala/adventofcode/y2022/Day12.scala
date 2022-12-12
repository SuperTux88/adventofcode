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

    printDayPart(1, getFewestSteps(List(start), end, heightmap).get, "Fewest step from start: %s")

    val allStarts = heightmap.filter(_._2 == 0).keys.toList
    printDayPart(2, getFewestSteps(allStarts, end, heightmap).get, "Fewest steps from any possible start: %s")
  }

  private def getFewestSteps(start: List[Pos], end: Pos, heightmap: Map[Pos, Int]): Option[Int] =
    Dijkstra(start, _ == end, getNeighbors(heightmap)) match {
      case (0, Nil) => None
      case (steps, _) => Some(steps)
    }

  private def getNeighbors(heightmap: Map[Pos, Int])(pos: Pos) =
    pos.directions.filter(n => heightmap.contains(n) && heightmap(n) - heightmap(pos) <= 1).map(n => (1, n))
}

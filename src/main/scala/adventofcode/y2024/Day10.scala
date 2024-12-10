package adventofcode.y2024

import adventofcode.common.pos.Pos

import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day10 extends Year2024 {
  override val day = 10

  override def runDay(input: BufferedSource): Unit = {
    val map = Pos.parseMap(input.getLines(), _.asDigit)
    val starts = map.filter(_._2 == 0).keySet.toList

    val trails = starts.par.map(findTrails(map))
    printDayPart(1, trails.map(_.toSet.size).sum, "Score of all trailheads: %s")
    printDayPart(2, trails.map(_.size).sum, "Score of all trailheads with distinct trails: %s")
  }

  private def findTrails(map: Map[Pos, Int])(start: Pos): Seq[Pos] =
    (1 to 9).foldLeft(Seq(start)) { (trails, height) =>
      trails.flatMap(nextUphill(map, _, height))
    }

  private def nextUphill(map: Map[Pos, Int], pos: Pos, height: Int): List[Pos] =
    pos.directions.filter(map.get(_).exists(_ == height))
}

package adventofcode.y2024

import adventofcode.Logging
import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day8 extends Year2024 {
  override val day = 8

  override def runDay(input: BufferedSource): Unit = {
    val map = Pos.parseMap(input.getLines(), identity)
    val antennas = map.filter(_._2 != '.').groupBy(_._2).values.map(_.keys.toSeq).toSeq
    val max = map.keys.max

    val antinodes = antennas.flatMap(getAntinodes).filter(inBounds(max)).toSet
    if (Logging.debug) Pos.printMap(mergeMap(map, antinodes), identity)

    printDayPart(1, antinodes.size, "Locations with antinodes: %s")

    val antinodesWithLines = antennas.flatMap(getAntinodesLines(_, inBounds(max))).toSet
    if (Logging.debug) Pos.printMap(mergeMap(map, antinodesWithLines), identity)

    printDayPart(2, antinodesWithLines.size, "Locations with antinodes in lines: %s")
  }

  private def getAntinodes(antennas: Seq[Pos]): Seq[Pos] =
    antennas.combinations(2).flatMap { case Seq(a, b) =>
      val diff = a - b
      Seq(a + diff, b - diff)
    }.toSeq

  private def getAntinodesLines(antennas: Seq[Pos], inBounds: Pos => Boolean): Seq[Pos] =
    antennas.combinations(2).flatMap { case Seq(a, b) =>
      val diff = a - b
      Iterator.iterate(a)(_ + diff).takeWhile(inBounds).toSeq
        ++ Iterator.iterate(b)(_ - diff).takeWhile(inBounds).toSeq
    }.toSeq

  private def inBounds(max: Pos)(pos: Pos): Boolean = pos.isInBounds(Pos(0, 0), max)

  private def mergeMap(map: Map[Pos, Char], antinodes: Set[Pos]): Map[Pos, Char] =
    antinodes.foldLeft(map) { case (map, p) =>
      map(p) match {
        case '.' => map.updated(p, '#')
        case _ => map
      }
    }
}

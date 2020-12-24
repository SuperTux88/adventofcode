package adventofcode.y2020

import adventofcode.common.pos.PosHex

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

object Day24 extends Year2020 {
  override val day = 24

  private val tilesPositions = input.getLines().map(line => readLine(line.toList)).toList
  private val blackTiles = tilesPositions.groupBy(identity).filter(_._2.size % 2 == 1).keySet

  printDayPart(1, blackTiles.size, "tiles with black side up: %s")

  private val blackTilesAfter100Days = runCycles(blackTiles)
  printDayPart(2, blackTilesAfter100Days.size, "tiles with black side up after 100 days: %s")

  @tailrec
  private def readLine(line: List[Char], pos: PosHex = PosHex(0, 0)): PosHex = line match {
    case Nil => pos
    case 'e' :: rest => readLine(rest, pos.east)
    case 'w' :: rest => readLine(rest, pos.west)
    case 'n' :: 'e' :: rest => readLine(rest, pos.northEast)
    case 'n' :: 'w' :: rest => readLine(rest, pos.northWest)
    case 's' :: 'e' :: rest => readLine(rest, pos.southEast)
    case 's' :: 'w' :: rest => readLine(rest, pos.southWest)
    case dir => throw new MatchError(s"Invalid direction $dir")
  }

  private def runCycles(map: Set[PosHex], cyclesToRun: Int = 100): Set[PosHex] =
    (1 to cyclesToRun).foldLeft(map) { (state, _) =>
      state.flatMap(_.neighbors).par.flatMap { pos =>
        (state.contains(pos), pos.neighbors.count(state.contains)) match {
          case (true, 1 | 2) => Some(pos)
          case (false, 2) => Some(pos)
          case _ => None
        }
      }.seq
    }
}

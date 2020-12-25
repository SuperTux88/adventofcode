package adventofcode.y2020

import adventofcode.common.MiscFunctions.conwaysGameOfLife
import adventofcode.common.pos.PosHex

import scala.annotation.tailrec

object Day24 extends Year2020 {
  override val day = 24

  private val blackTiles = input.getLines().foldLeft(Set.empty[PosHex]) { (blackTiles, line) =>
    val pos = readLine(line.toList)
    if (blackTiles.contains(pos)) blackTiles - pos else blackTiles + pos
  }

  printDayPart(1, blackTiles.size, "tiles with black side up: %s")

  printDayPart(2, conwaysGameOfLife(blackTiles, 100, nextState).size,
    "tiles with black side up after 100 days: %s")

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

  private def nextState(isBlack: Boolean, blackNeighbors: Int) = (isBlack, blackNeighbors) match {
    case (true, 1 | 2) | (false, 2) => true
    case _ => false
  }
}

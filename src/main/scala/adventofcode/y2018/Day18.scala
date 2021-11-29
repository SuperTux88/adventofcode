package adventofcode.y2018

import adventofcode.common.pos.Direction

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day18 extends Year2018 {
  override val day = 18

  private type Acres = Vector[Vector[Char]]

  override def runDay(input: BufferedSource): Unit = {
    val map = input.getLines().map(_.toVector).toVector

    val map10 = (1 to 10).foldLeft(map) { (currentMap, _) => transform(currentMap) }

    printDayPart(1, count(map10, '|') * count(map10, '#'))

    val (seenStates, repeatOffset, repeatPattern) = findRepeatOffset(map10, 11)

    val resultOffset = (1000000000 - repeatOffset) % repeatPattern + repeatOffset
    printDayPart(2, count(seenStates(resultOffset), '|') * count(seenStates(resultOffset), '#'))
  }

  @tailrec
  private def findRepeatOffset(map: Acres, minute: Int, seenStates: Map[Acres, Int] = Map.empty): (Map[Int, Acres], Int, Int) = {
    val nextMap = transform(map)
    val repeatOffset = seenStates.getOrElse(nextMap, 0)
    if (repeatOffset > 0) {
      (seenStates.filter(_._2 >= repeatOffset).map(_.swap), repeatOffset, minute - repeatOffset)
    } else {
      findRepeatOffset(nextMap, minute + 1, seenStates + (nextMap -> minute))
    }
  }

  private def transform(map: Acres): Acres = map.zipWithIndex.map {
    case (line, y) =>
      line.zipWithIndex.map {
        case (acre, x) =>
          val adjacents = getAdjacents(map, x, y)
          acre match {
            case '.' if adjacents('|') >= 3 => '|'
            case '|' if adjacents('#') >= 3 => '#'
            case '#' if adjacents('#') < 1 || adjacents('|') < 1 => '.'
            case _ => acre
          }
    }
  }

  private def getAdjacents(map: Acres, x: Int, y: Int) =
    Direction.directionsWithDiagonals.flatMap {
      case (xOffset, yOffset) => map.lift(y + yOffset) match {
        case Some(line) => line.lift(x + xOffset)
        case None => None
      }
    }.groupBy(identity).view.mapValues(_.size).toMap.withDefaultValue(0)

  private def count(map: Acres, char: Char) = map.flatten.count(_ == char)
}

package adventofcode.y2018

import scala.annotation.tailrec

object Day18 extends Year2018 {
  override val day = 18

  type Acres = List[List[Char]]

  private val acreOffsets = Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

  private val map = input.getLines.map(_.toList).toList

  private val map10 = (1 to 10).foldLeft(map) { (currentMap, _) => transform(currentMap) }

  printDayPart(1, count(map10, '|') * count(map10, '#'))

  val (seenStates, repeatOffset, repeatPattern) = findRepeatOffset(map10, 11)

  val resultOffset = (1000000000 - repeatOffset) % repeatPattern + repeatOffset
  printDayPart(2, count(seenStates(resultOffset), '|') * count(seenStates(resultOffset), '#'))

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
    acreOffsets.flatMap {
      case (xOffset, yOffset) => map.lift(y + yOffset) match {
        case Some(line) => line.lift(x + xOffset)
        case None => None
      }
    }.groupBy(identity).mapValues(_.size).withDefaultValue(0)

  private def count(map: Acres, char: Char) = map.flatten.count(_ == char)
}

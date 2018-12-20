package adventofcode.y2018

import adventofcode.Logging

import scala.annotation.tailrec

object Day20 extends Year2018 {
  override val day = 20

  private val map = process(input.drop(1))._1

  printMap(map)

  private val distances = walkMap(map.filter(_._2 == '.').keys.toSet, map)

  printDayPart(1, distances.maxBy(_._2)._2)
  printDayPart(2, distances.count(_._2 >= 1000))

  private def process(regex: Iterator[Char], map: Map[Pos, Char] = Map(Pos(0, 0) -> 'X'), positions: Set[Pos] = Set(Pos(0, 0))): (Map[Pos, Char], Set[Pos], Char) = {
    regex.next match {
      case direction@('N'|'E'|'S'|'W') =>
        val (nextMap, nextPos) = positions.foldLeft(map, Set[Pos]()) { (state, pos) =>
          val dir = Dir.directions(direction)
          val doorPos = pos + dir.dir
          val nextPos = doorPos + dir.dir
          (state._1 + (doorPos -> dir.doorChar) + (nextPos -> '.'), state._2 + nextPos)
        }
        process(regex, nextMap, nextPos)
      case '(' =>
        var (nextMap, nextPos, endChar) = process(regex, map, positions)
        var branch = endChar == '|'
        while(branch) {
          val (nextMap2, nextPos2, endChar2) = process(regex, nextMap, positions)
          nextMap = nextMap2
          nextPos ++= nextPos2
          branch = endChar2 == '|'
        }
        process(regex, nextMap, nextPos)
      case ret@('|'|')'|'$') => (map, positions, ret)
    }
  }

  private case class Pos(x: Int, y: Int) {
    def +(direction: (Int, Int)): Pos = Pos(x + direction._1, y + direction._2)
  }

  private object Pos {
    val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))
  }

  private case class Dir(dir: (Int, Int), doorChar: Char)

  private object Dir {
    val directions: Map[Char, Dir] = Map(
      'N' -> Dir(( 0, -1), '-'),
      'E' -> Dir(( 1,  0), '|'),
      'S' -> Dir(( 0,  1), '-'),
      'W' -> Dir((-1,  0), '|'),
    )
  }

  private def walkMap(rooms: Set[Pos], map: Map[Pos, Char]): Map[Pos, Int] = {
    @tailrec
    def step(toVisit: Set[Pos], current: Set[Pos], steps: Int = 1, roomSteps: Map[Pos, Int] = Map.empty): Map[Pos, Int] = {
      val nextSteps = current.flatMap { pos =>
        Pos.directions.filter { dir =>
          Set('-', '|').contains(map.getOrElse(pos + dir, '#'))
        }.map(dir => pos + dir + dir).filter(toVisit.contains)
      }

      val nextToVisit = toVisit -- nextSteps
      val nextRoomSteps = nextSteps.foldLeft(roomSteps) { (nextRoomSteps, pos) => nextRoomSteps + (pos -> steps) }
      if (nextToVisit.isEmpty)
        nextRoomSteps
      else
        step(nextToVisit, nextSteps, steps + 1, nextRoomSteps)
    }

    step(rooms, Set(Pos(0, 0)))
  }

  private def printMap(map: Map[Pos, Char]): Unit = if (Logging.debug) {
    val (minX, maxX, minY, maxY) = (map.minBy(_._1.x)._1.x, map.maxBy(_._1.x)._1.x, map.minBy(_._1.y)._1.y, map.maxBy(_._1.y)._1.y)
    (minY - 1 to maxY + 1).foreach { y =>
      println((minX - 1 to maxX + 1).map(x => map.getOrElse(Pos(x, y), "#")).mkString)
    }
  }
}

package adventofcode.y2018

import adventofcode.Logging
import adventofcode.common.MapImplicits.MapImplicits
import adventofcode.common.pos.{Direction, Pos}

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day20 extends Year2018 {
  override val day = 20

  override def runDay(input: BufferedSource): Unit = {
    val map = process(input.drop(1))

    if (Logging.debug) Pos.printMap(map.withDefaultValue('#'), identity)

    val distances = walkMap(map.keySetByValue('.'), map)

    printDayPart(1, distances.maxBy(_._2)._2)
    printDayPart(2, distances.count(_._2 >= 1000))
  }

  @tailrec
  private def process(
                       regex: Iterator[Char],
                       map: Map[Pos, Char] = Map(Pos.zero -> 'X'),
                       positions: Set[Pos] = Set(Pos.zero),
                       branches: List[Set[Pos]] = List(Set(Pos.zero)),
                       branchPositions: List[Set[Pos]] = List.empty
                     ): Map[Pos, Char] = {
    regex.next() match {
      case direction@('N'|'E'|'S'|'W') =>
        val (nextMap, nextPos) = positions.foldLeft(map, Set[Pos]()) { (state, pos) =>
          val dir = Dir.directions(direction)
          val doorPos = pos + dir.dir
          val nextPos = doorPos + dir.dir
          (state._1 + (doorPos -> dir.doorChar) + (nextPos -> '.'), state._2 + nextPos)
        }
        process(regex, nextMap, nextPos, branches, branchPositions)
      case '(' =>
        process(regex, map, positions, positions :: branches, Set[Pos]() :: branchPositions)
      case '|' =>
        process(regex, map, branches.head, branches, (branchPositions.head ++ positions) :: branchPositions.tail)
      case ')' =>
        process(regex, map, branchPositions.head ++ positions, branches.tail, branchPositions.tail)
      case '$' => map
    }
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
        Direction.directions.filter { dir =>
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

    step(rooms, Set(Pos.zero))
  }
}

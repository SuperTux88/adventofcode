package adventofcode.y2024

import adventofcode.common.pos.Direction.DirectionPos
import adventofcode.common.pos.{Direction, Pos}

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day6 extends Year2024 {
  override val day = 6

  override def runDay(input: BufferedSource): Unit = {
    val map = Pos.parseMap(input.getLines(), identity)
    val startPos = map.find { case (_, c) => c == '^' }.get._1
    val obstructions = map.filter { case (_, c) => c == '#' }.keySet
    val max = map.keys.max

    val path = findPath(obstructions, inBounds(max), startPos, Direction.up)
    printDayPart(1, path.size, "Visited positions before leaving the map: %s")

    val loops = path.toSeq.par.count { pos =>
      findLoop(obstructions + pos, inBounds(max), startPos, Direction.up)
    }
    printDayPart(2, loops, "Possible obstructions to make the guard loop: %s")
  }

  private def findPath(obstructions: Set[Pos], boundsCheck: Pos => Boolean, startPos: Pos, direction: Pos): Set[Pos] = {
    @tailrec
    def inner(pos: Pos, dir: Pos, path: Set[Pos]): Set[Pos] = {
      val newPos = pos + dir
      if (!boundsCheck(newPos)) {
        path
      } else if (obstructions.contains(newPos)) {
        inner(pos, dir.rotateRight, path)
      } else {
        inner(newPos, dir, path + newPos)
      }
    }

    inner(startPos, direction, Set(startPos))
  }

  private def findLoop(obstructions: Set[Pos], boundsCheck: Pos => Boolean, startPos: Pos, direction: Pos): Boolean = {
    @tailrec
    def inner(pos: Pos, dir: Pos, path: Set[(Pos, Pos)]): Boolean = {
      val newPos = pos + dir
      if (path.contains((newPos, dir))) {
        true
      } else if (!boundsCheck(newPos)) {
        false
      } else if (obstructions.contains(newPos)) {
        inner(pos, dir.rotateRight, path)
      } else {
        inner(newPos, dir, path + ((newPos, dir)))
      }
    }

    inner(startPos, direction, Set((startPos, direction)))
  }

  private def inBounds(max: Pos)(pos: Pos) = pos.x >= 0 && pos.y >= 0 && pos.x <= max.x && pos.y <= max.y
}

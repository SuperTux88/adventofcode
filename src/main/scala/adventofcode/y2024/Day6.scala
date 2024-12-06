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

    val path = findPath(map, startPos, Direction.up)
    printDayPart(1, path.size, "Visited positions before leaving the map: %s")

    val loops = path.toSeq.par.count { pos =>
      findLoop(map.updated(pos, '#'), startPos, Direction.up)
    }
    printDayPart(2, loops, "Possible obstructions to make the guard loop: %s")
  }

  private def findPath(map: Map[Pos, Char], startPos: Pos, direction: Pos): Set[Pos] = {
    @tailrec
    def inner(pos: Pos, dir: Pos, path: Set[Pos]): Set[Pos] = {
      val newPos = pos + dir
      map.get(newPos) match {
        case None => path
        case Some('#') => inner(pos, dir.rotateRight, path)
        case _ => inner(newPos, dir, path + newPos)
      }
    }

    inner(startPos, direction, Set(startPos))
  }

  private def findLoop(map: Map[Pos, Char], startPos: Pos, direction: Pos): Boolean = {
    @tailrec
    def inner(pos: Pos, dir: Pos, path: Set[(Pos, Pos)]): Boolean = {
      val newPos = pos + dir
      if (path.contains((newPos, dir))) {
        true
      } else {
        map.get(newPos) match {
          case None => false
          case Some('#') => inner(pos, dir.rotateRight, path)
          case _ => inner(newPos, dir, path + ((newPos, dir)))
        }
      }
    }

    inner(startPos, direction, Set((startPos, direction)))
  }
}

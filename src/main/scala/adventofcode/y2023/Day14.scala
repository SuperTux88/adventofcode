package adventofcode.y2023

import adventofcode.common.pos.{Direction, Pos}

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day14 extends Year2023 {
  override val day = 14

  private val CYCLE = List(Direction.up, Direction.left, Direction.down, Direction.right)
  private val TOTAL_CYCLES = 1000000000

  override def runDay(input: BufferedSource): Unit = {
    val rocks = Pos.parseMap(input.getLines(), {
      case '.' => Shape.Empty
      case 'O' => Shape.Round
      case '#' => Shape.Cube
    })
    val size = rocks.keys.max

    val load = calculateLoad(rollDirection(rocks, size, Direction.up), size)
    printDayPart(1, load, "Total load on north support beams: %s")

    val (loopStart, loopEnd, afterLoop) = findLoop(rocks, size)
    val loopSize = loopEnd - loopStart
    val remainingCycles = (TOTAL_CYCLES - loopStart) % loopSize
    printDebug(s"Loop starts at $loopStart and ends at $loopEnd, size is $loopSize, remaining cycles are $remainingCycles")
    val loadCycles = calculateLoad(loop(afterLoop, size, remainingCycles), size)

    printDayPart(2, loadCycles, "Total load after 1000000000 cycles: %s")
  }

  @tailrec
  private def findLoop(rocks: Map[Pos, Shape], size: Pos, seen: Map[Set[Pos], Int] = Map.empty, round: Int = 0): (Int, Int, Map[Pos, Shape]) = {
    val next = cycle(rocks, size)
    val roundRocks = next.filter(_._2 == Shape.Round).keySet
    if (seen.contains(roundRocks))
      (seen(roundRocks) + 1, round + 1, next)
    else
      findLoop(next, size, seen.updated(roundRocks, round), round + 1)
  }

  private def loop(rocks: Map[Pos, Shape], size: Pos, times: Int) =
    (1 to times).foldLeft(rocks) { (rocks, _) => cycle(rocks, size) }

  private def cycle(rocks: Map[Pos, Shape], size: Pos) =
    CYCLE.foldLeft(rocks) { (rocks, direction) => rollDirection(rocks, size, direction) }

  private def rollDirection(rocks: Map[Pos, Shape], size: Pos, direction: Pos) = {
    val xDirection = if (direction.x > 0) size.x to 0 by -1 else 0 to size.x
    val yDirection = if (direction.y > 0) size.y to 0 by -1 else 0 to size.y
    xDirection.foldLeft(rocks) { (rocks, x) =>
      yDirection.foldLeft(rocks) { (rocks, y) =>
        val pos = Pos(x, y)
        rocks(pos) match {
          case Shape.Round => roll(rocks.updated(pos, Shape.Empty), pos, direction)
          case _ => rocks
        }
      }
    }
  }

  @tailrec
  private def roll(rocks: Map[Pos, Shape], pos: Pos, direction: Pos): Map[Pos, Shape] = {
    val newPos = pos + direction
    if (rocks.getOrElse(newPos, Shape.Cube) == Shape.Empty)
      roll(rocks, newPos, direction)
    else
      rocks.updated(pos, Shape.Round)
  }

  private def calculateLoad(rocks: Map[Pos, Shape], size: Pos) =
    rocks.filter(_._2 == Shape.Round).keys.toSeq.map(size.y - _.y + 1).sum

  private enum Shape {
    case Round, Cube, Empty
  }
}

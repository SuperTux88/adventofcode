package adventofcode.y2023

import adventofcode.Logging
import adventofcode.common.pos.{Direction, Pos}
import adventofcode.common.search.AStar

import scala.io.AnsiColor.{RED, RESET}
import scala.io.BufferedSource

object Day17 extends Year2023 {
  override val day = 17

  override def runDay(input: BufferedSource): Unit = {
    val map = Pos.parseMap(input.getLines(), _.asDigit)
    val start = (Pos.zero, Direction.rightIndex, 0)
    val target = map.keys.max

    val (heatLoss, path) = AStar(start, _._1 == target, getNextSteps(map, target, 1, 3))
    if (Logging.debug) printMap(map, path)

    printDayPart(1, heatLoss, "Minimal heat loss: %s")

    val (heatLoss2, path2) = AStar(
      start,
      (pos, dir, steps) => pos == target && steps >= 4,
      getNextSteps(map, target, 4, 10)
    )
    if (Logging.debug) printMap(map, path2)

    printDayPart(2, heatLoss2, "Minimal heat loss with ultra crucibles: %s")
  }

  private def getNextSteps(map: Map[Pos, Int], target: Pos, minSteps: Int, maxSteps: Int)
                          (pos: Pos, dir: Int, steps: Int): List[(Int, Int, (Pos, Int, Int))] = {
    val possibleDirections = if (steps < minSteps) {
      List((dir, steps + 1))
    } else if (steps >= maxSteps) {
      List((Direction.rotateLeft(dir), 1), (Direction.rotateRight(dir), 1))
    } else {
      List((dir, steps + 1), (Direction.rotateLeft(dir), 1), (Direction.rotateRight(dir), 1))
    }

    possibleDirections.flatMap { (dir, steps) =>
      val newPos = pos.moveDirectionIndex(dir)
      map.get(newPos).map(value => (value, newPos.distance(target), (newPos, dir, steps)))
    }
  }

  private def printMap(map: Map[Pos, Int], path: List[(Pos, Int, Int)]): Unit = {
    val directions = path.map((pos, dir, _) => pos -> (dir match {
      case 0 => '^'
      case 1 => '>'
      case 2 => 'v'
      case 3 => '<'
    })).toMap
    Pos.printMapArea(Pos.zero, map.keys.max,
      pos => directions.get(pos).map(d => s"$RED$d$RESET").getOrElse(map(pos).toString))
  }
}

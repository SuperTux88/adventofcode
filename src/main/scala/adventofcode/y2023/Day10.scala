package adventofcode.y2023

import adventofcode.Logging
import adventofcode.common.MapImplicits.MapImplicits
import adventofcode.common.pos.{Direction, Pos}

import scala.annotation.tailrec
import scala.io.AnsiColor.{CYAN, RED, RESET}
import scala.io.BufferedSource

object Day10 extends Year2023 {
  override val day = 10

  private val PIPES = Map(
    '|' -> Map(
      Direction.up -> Direction.up,
      Direction.down -> Direction.down
    ),
    '-' -> Map(
      Direction.left -> Direction.left,
      Direction.right -> Direction.right
    ),
    'L' -> Map(
      Direction.left -> Direction.up,
      Direction.down -> Direction.right
    ),
    'J' -> Map(
      Direction.right -> Direction.up,
      Direction.down -> Direction.left
    ),
    '7' -> Map(
      Direction.right -> Direction.down,
      Direction.up -> Direction.left
    ),
    'F' -> Map(
      Direction.left -> Direction.down,
      Direction.up -> Direction.right
    )
  )

  override def runDay(input: BufferedSource): Unit = {
    val map = Pos.parseMap(input.getLines(), identity)
    val size = map.keys.max
    val start = map.findKeyByValue('S').get

    val mapPipes = map.flatMap((pos, char) => PIPES.get(char).map(pos -> _))
    val pipeLoop = findLoop(mapPipes, start)
    printDayPart(1, pipeLoop.size / 2, "Steps to point farthest away from start: %s")

    val doublePipes = mapPipes.filter { (pos, pipe) => pipeLoop.contains(pos) }.flatMap { (pos, pipe) =>
      val doublePos = pos * 2
      pipe.values.map(doublePos + _).toSet + doublePos
    }.toSet + (start * 2) // start is missing from mapPipes
    val doubleOutside = findOutside(doublePipes, (size * 2).down.right, Set(Pos.zero.up.left))
    val inside = map.keySet.diff(doubleOutside.map(_ / 2)).diff(pipeLoop)

    if (Logging.debug) printMap(size, map, pipeLoop, inside)
    printDayPart(2, inside.size, "Tiles enclosed by loop: %s")
  }

  private def findLoop(pipes: Map[Pos, Map[Pos, Pos]], start: Pos): Set[Pos] = {
    @tailrec
    def loop(seen: Set[Pos], current: List[(Pos, Pos)]): Set[Pos] = {
      val next = current.map { (pos, direction) =>
        val nextDir = pipes(pos)(direction)
        (pos + nextDir, nextDir)
      }
      val nextPos = next.map(_._1).toSet
      if (nextPos.size == 1) seen ++ nextPos
      else loop(seen ++ nextPos, next)
    }

    val startDirections = Direction.directions.map(dir => (start + dir, Pos(dir))).flatMap((pos, dir) => {
      pipes.get(pos).flatMap(_.get(dir).map(_ => (pos, dir)))
    })
    loop(startDirections.map(_._1).toSet + start, startDirections)
  }

  @tailrec
  private def findOutside(pipe: Set[Pos], size: Pos, current: Set[Pos], seen: Set[Pos] = Set.empty): Set[Pos] = {
    val next = current.flatMap(_.directions).diff(seen).diff(pipe).filter { pos =>
      pos.x >= -1 && pos.y >= -1 && pos.x <= size.x && pos.y <= size.y
    }
    if (next.isEmpty) seen
    else findOutside(pipe, size, next, seen ++ next)
  }

  private def printMap[V](size: Pos, map: Map[Pos, Char], pipes: Set[Pos], inside: Set[Pos]): Unit =
    (0 to size.y).foreach(y => println((0 to size.x).map(x => Pos(x, y) match {
      case pos if pipes.contains(pos) => map(pos) match {
        case '|' => '┃'
        case '-' => '━'
        case 'L' => '┗'
        case 'J' => '┛'
        case '7' => '┓'
        case 'F' => '┏'
        case 'S' => 'S'
      }
      case pos => (if (inside.contains(pos)) CYAN else RED) + '█' + RESET
    }).mkString))
}

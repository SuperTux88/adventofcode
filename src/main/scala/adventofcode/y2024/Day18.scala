package adventofcode.y2024

import adventofcode.Logging
import adventofcode.common.pos.Pos
import adventofcode.common.search.Dijkstra

import scala.io.BufferedSource
import scala.io.AnsiColor.{CYAN, RED, RESET}

object Day18 extends Year2024 {
  override val day = 18

  // private val SIZE = Pos(6, 6)
  // private val BYTES = 12

  private val SIZE = Pos(70, 70)
  private val BYTES = 1024

  override def runDay(input: BufferedSource): Unit = {
    val bytes = input.getLines().takeWhile(_.nonEmpty).map { line =>
      line.split(",") match {
        case Array(x, y) => Pos(x.toInt, y.toInt)
      }
    }.toList

    val firstBytes = bytes.take(BYTES)
    val (_, steps) = Dijkstra(Pos(0, 0), _ == SIZE, next(firstBytes))
    printMap(firstBytes, steps)
    printDayPart(1, steps.size - 1, "P1: %s")

    val index = bytes.indices.findLast { i =>
      Dijkstra(Pos(0, 0), _ == SIZE, next(bytes.take(i)))._2.nonEmpty
    }.get
    val blocking = bytes(index)
    if (Logging.debug)
      printMap(bytes.take(index), Dijkstra(Pos(0, 0), _ == SIZE, next(bytes.take(index)))._2, Some(blocking))
    printDayPart(2, blocking.toString, "P2: %s")
  }

  private def next(bytes: List[Pos])(pos: Pos): List[(Int, Pos)] =
    pos.directions.filter { next =>
      next.x >= 0 && next.x <= SIZE.x && next.y >= 0 && next.y <= SIZE.y && !bytes.contains(next)
    }.map((1, _))

  private def printMap(bytes: List[Pos], steps: List[Pos], blocking: Option[Pos] = None): Unit = if (Logging.debug)
    for (y <- 0 to SIZE.y) {
      for (x <- 0 to SIZE.x) {
        print(
          if (bytes.contains(Pos(x, y))) '#'
          else if (blocking.contains(Pos(x, y))) s"${RED}X${RESET}"
          else if (steps.contains(Pos(x, y))) s"${CYAN}O${RESET}"
          else '.'
        )
      }
      println()
    }
}

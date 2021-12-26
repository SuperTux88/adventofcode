package adventofcode.y2021

import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day25 extends Year2021 {
  override val day = 25

  override def runDay(input: BufferedSource): Unit = {
    val grid = input.getLines().map(_.toVector).toVector

    printDayPart(1, steps(grid), "first step on which no sea cucumbers move: %s")
  }

  @tailrec
  private def steps(grid: Vector[Vector[Char]], step: Int = 1): Int = {
    val newGrid = grid.map(move(_, '>')).transpose.map(move(_, 'v')).transpose

    if (grid == newGrid)
      step
    else
      steps(newGrid, step + 1)
  }

  private def move(line: Vector[Char], direction: Char): Vector[Char] =
    line.zipWithIndex.map {
      case ('.', index) if line((index + line.size - 1) % line.size) == direction => direction
      case (`direction`, index) if line((index + 1) % line.size) == '.' => '.'
      case (c, _) => c
    }
}

package adventofcode.y2021

import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day11 extends Year2021 {
  override val day = 11

  override def runDay(input: BufferedSource): Unit = {
    val octopuses = Pos.parseMap(input.getLines().takeWhile(_.nonEmpty), _.asDigit)

    val (flashesAfter100, octopusesAfter100) = (1 to 100).foldLeft(0, octopuses) {
      case ((flashCounter, currentOctopuses), _) =>
        val (flashedInRound, newOctopuses) = step(currentOctopuses)
        (flashCounter + flashedInRound, newOctopuses)
    }

    printDayPart(1, flashesAfter100, "there are %s flashes in the first 100 steps")
    printDayPart(2, findSync(octopusesAfter100, 100), "first step during which all octopuses are in sync: %s")
  }

  private def step(octopuses: Map[Pos, Int]): (Int, Map[Pos, Int]) = {
    @tailrec
    def cycle(state: Map[Pos, Int], flashed: Int = 0): (Int, Map[Pos, Int]) = {
      val nowFlashing = state.filter(_._2 > 9).keys.toSet
      if (nowFlashing.isEmpty) {
        (flashed, state)
      } else {
        val newState = nowFlashing.foldLeft(state) { (currentState, pos) =>
          pos.neighbors.foldLeft(currentState) { (currentState, neighbor) =>
            currentState.get(neighbor) match {
              case None => currentState
              case Some(0) => currentState
              case Some(value) => currentState.updated(neighbor, value + 1)
            }
          }.updated(pos, 0)
        }
        cycle(newState, flashed + nowFlashing.size)
      }
    }

    cycle(octopuses.view.mapValues(_ + 1).toMap)
  }

  @tailrec
  private def findSync(octopuses: Map[Pos, Int], steps: Int): Int =
    step(octopuses) match {
      case (100, _) => steps + 1
      case (_, nextOctopuses) => findSync(nextOctopuses, steps + 1)
    }
}
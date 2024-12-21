package adventofcode.y2023

import adventofcode.common.MapImplicits.MapImplicits
import adventofcode.common.pos.{Direction, Pos}

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day14 extends Year2023 {
  override val day = 14

  private val CYCLE = List(Direction.up, Direction.left, Direction.down, Direction.right)
  private val TOTAL_CYCLES = 1000000000

  override def runDay(input: BufferedSource): Unit = {
    val rocks = Pos.parseMap(input.getLines(), identity)
    val size = rocks.keys.max.down.right

    val blocked = rocks.keySetByValue('#') ++ Pos.zero.border(size)
    val round = rocks.keySetByValue('O')
    val platform = Platform(blocked)

    val rolledUp = platform.rollDirection(round, Direction.up)
    printDayPart(1, calculateLoad(rolledUp, size.y), "Total load on north support beams: %s")

    val (loopStart, loopEnd, afterLoop) = platform.findLoop(round)
    val loopSize = loopEnd - loopStart
    val remainingCycles = (TOTAL_CYCLES - loopStart) % loopSize
    printDebug(s"Loop starts at $loopStart and ends at $loopEnd, size is $loopSize, remaining cycles are $remainingCycles")
    val afterCycles = platform.loop(afterLoop, remainingCycles)

    printDayPart(2, calculateLoad(afterCycles, size.y), "Total load after 1000000000 cycles: %s")
  }

  private def calculateLoad(round: Set[Pos], height: Int) = round.toSeq.map(height - _.y).sum

  private case class Platform(blocked: Set[Pos]) {
    @tailrec
    final def findLoop(round: Set[Pos], seen: Map[Set[Pos], Int] = Map.empty, cycleCount: Int = 0): (Int, Int, Set[Pos]) = {
      val next = cycle(round)
      if (seen.contains(next))
        (seen(next) + 1, cycleCount + 1, next)
      else
        findLoop(next, seen.updated(next, cycleCount), cycleCount + 1)
    }

    def loop(round: Set[Pos], times: Int): Set[Pos] =
      (1 to times).foldLeft(round) { (round, _) => cycle(round) }

    private def cycle(round: Set[Pos]) =
      CYCLE.foldLeft(round) { (round, direction) => rollDirection(round, direction) }

    def rollDirection(round: Set[Pos], direction: Pos): Set[Pos] = {
      val sortOrder = direction * -1
      round.toSeq.sortBy(_ * sortOrder).foldLeft(Set.empty)(roll(_, _, direction))
    }

    @tailrec
    private def roll(round: Set[Pos], pos: Pos, direction: Pos): Set[Pos] = {
      val newPos = pos + direction
      if (blocked.contains(newPos) || round.contains(newPos))
        round + pos
      else
        roll(round, newPos, direction)
    }
  }
}

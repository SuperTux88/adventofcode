package adventofcode.y2015

import adventofcode.common.MiscFunctions.conwaysGameOfLife
import adventofcode.common.pos.Pos

object Day18 extends Year2015 {
  override val day: Int = 18

  private val initialGrid = Pos.parseMap(input.getLines(), char => char == '#').filter(_._2).keySet
  private val emptyGrid = (for {x <- 0 to 99; y <- 0 to 99} yield Pos(x, y)).toSet
  private val nextGrid = (_: Set[Pos], _: Int) => emptyGrid

  printDayPart(1, conwaysGameOfLife(initialGrid, 100, nextState, nextPositions = nextGrid).size,
    "total lights on after 100 steps: %s")

  private val initialGridWithStuckLights = initialGrid + Pos(0, 0) + Pos(0, 99) + Pos(99, 99) + Pos(99, 0)
  printDayPart(2, conwaysGameOfLife(initialGridWithStuckLights, 100, nextStateWithStuckLights, nextStateValueForStuckLights, nextGrid).size,
    "total lights on with stuck lights: %s")

  private def nextState(on: Boolean, neighbors: Int) = (on, neighbors) match {
    case (true, 2 | 3) | (false, 3) => true
    case _ => false
  }

  private def nextStateValueForStuckLights(pos: Pos, state: Set[Pos]) =
    (pos, pos.neighbors.count(state.contains))

  private def nextStateWithStuckLights(on: Boolean, pos: (Pos, Int)) = (on, pos) match {
    case (_, (Pos(0, 0) | Pos(0, 99) | Pos(99, 99) | Pos(99, 0), _)) => true
    case (on, (_, activeNeighbors)) => nextState(on, activeNeighbors)
  }
}

package adventofcode.y2015

import adventofcode.common.misc.ConwaysGameOfLife
import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day18 extends Year2015 {
  override val day: Int = 18

  override def runDay(input: BufferedSource): Unit = {
    val initialGrid = Pos.parseMap(input.getLines(), char => char == '#').filter(_._2).keySet
    val emptyGrid = (for (x <- 0 to 99; y <- 0 to 99) yield Pos(x, y)).toSet
    val nextGrid = (_: Set[Pos], _: Int) => emptyGrid

    printDayPart(1, ConwaysGameOfLife.run(initialGrid, 100, nextState, nextGrid).size,
      "total lights on after 100 steps: %s")

    val initialGridWithStuckLights = initialGrid + Pos(0, 0) + Pos(0, 99) + Pos(99, 99) + Pos(99, 0)
    val totalWithStuckLights = ConwaysGameOfLife.run(initialGridWithStuckLights, 100, nextStateWithStuckLights, nextGrid, nextStateValueForStuckLights).size
    printDayPart(2, totalWithStuckLights, "total lights on with stuck lights: %s")
  }

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

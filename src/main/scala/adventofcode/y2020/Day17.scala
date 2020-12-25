package adventofcode.y2020

import adventofcode.common.MiscFunctions.conwaysGameOfLife
import adventofcode.common.pos.{Pos, Pos3D, Pos4D}

object Day17 extends Year2020 {
  override val day = 17

  private val initialSlice = Pos.parseMap(input.getLines(), char => char == '#').filter(_._2).keySet

  private val map3D = initialSlice.map(p => Pos3D(p.x, p.y, 0))
  printDayPart(1, conwaysGameOfLife(map3D, 6, nextState).size,
    "active cubes after 6 cycles: %s")

  private val map4D = initialSlice.map(p => Pos4D(p.x, p.y, 0, 0))
  printDayPart(2, conwaysGameOfLife(map4D, 6, nextState).size,
    "active cubes after 6 cycles with 4 dimensions: %s")

  private def nextState(isActive: Boolean, activeNeighbors: Int) =
    (isActive, activeNeighbors) match {
      case (true, 2 | 3) => true
      case (false, 3) => true
      case _ => false
    }
}

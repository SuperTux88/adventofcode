package adventofcode.y2020

import adventofcode.common.misc.ConwaysGameOfLife
import adventofcode.common.pos.{Pos, Pos3D, Pos4D}

import scala.io.BufferedSource

object Day17 extends Year2020 {
  override val day = 17

  override def runDay(input: BufferedSource): Unit = {
    val initialSlice = Pos.parseMap(input.getLines(), char => char == '#').filter(_._2).keySet

    val map3D = initialSlice.map(p => Pos3D(p.x, p.y, 0))
    printDayPart(1, ConwaysGameOfLife.run(map3D, 6, nextState, ConwaysGameOfLife.neighbors).size,
      "active cubes after 6 cycles: %s")

    val map4D = initialSlice.map(p => Pos4D(p.x, p.y, 0, 0))
    printDayPart(2, ConwaysGameOfLife.run(map4D, 6, nextState, ConwaysGameOfLife.neighbors).size,
      "active cubes after 6 cycles with 4 dimensions: %s")
  }

  private def nextState(isActive: Boolean, activeNeighbors: Int) =
    (isActive, activeNeighbors) match {
      case (true, 2 | 3) => true
      case (false, 3) => true
      case _ => false
    }
}

package adventofcode.y2020

import adventofcode.common.pos.Pos

object Day3 extends Year2020 {
  override val day = 3

  private val map = input.getLines().zipWithIndex.flatMap {
    case (line, y) => line.zipWithIndex.map {
      case (char, x) => Pos(x, y) -> (char == '#')
    }
  }.toMap

  private val (width, height) = (map.maxBy(_._1.x)._1.x + 1, map.maxBy(_._1.y)._1.y + 1)

  private val treesOnSlope = getTreesOnSlope(Pos(3, 1))

  printDayPart(1, treesOnSlope, "trees on slope: %s")

  private val additionalSlopes = List(Pos(1, 1), Pos(5, 1), Pos(7, 1), Pos(1, 2))
  private val treesOnAdditionalSlopes = additionalSlopes.map(getTreesOnSlope)

  printDayPart(2, treesOnAdditionalSlopes.product * treesOnSlope, "product of trees on slopes: %s")

  private def positionHasTree(pos: Pos) =
    map(Pos(pos.x % width, pos.y))

  private def getTreesOnSlope(slopeDirection: Pos): Long =
    (0 until height / slopeDirection.y).map(slopeDirection * _).count(positionHasTree)
}

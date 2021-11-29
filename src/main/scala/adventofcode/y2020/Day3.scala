package adventofcode.y2020

import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day3 extends Year2020 {
  override val day = 3

  override def runDay(input: BufferedSource): Unit = {
    val forest = Forest(Pos.parseMap(input.getLines(), char => char == '#'))

    val treesOnSlope = getTreesOnSlope(Pos(3, 1))(forest)

    printDayPart(1, treesOnSlope, "trees on slope: %s")

    val additionalSlopes = List(Pos(1, 1), Pos(5, 1), Pos(7, 1), Pos(1, 2))
    val treesOnAdditionalSlopes = additionalSlopes.map(getTreesOnSlope(_)(forest))

    printDayPart(2, treesOnAdditionalSlopes.product * treesOnSlope, "product of trees on slopes: %s")
  }

  private def positionHasTree(pos: Pos)(implicit trees: Forest) =
    trees.forest(Pos(pos.x % trees.width, pos.y))

  private def getTreesOnSlope(slopeDirection: Pos)(implicit trees: Forest): Long =
    (0 until trees.height / slopeDirection.y).map(slopeDirection * _).count(positionHasTree)

  private case class Forest(forest: Map[Pos, Boolean]) {
    val (width, height) = (forest.maxBy(_._1.x)._1.x + 1, forest.maxBy(_._1.y)._1.y + 1)
  }
}

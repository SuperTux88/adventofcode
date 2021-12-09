package adventofcode.y2021

import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day9 extends Year2021 {
  override val day = 9

  override def runDay(input: BufferedSource): Unit = {
    val heightmap = Pos.parseMap(input.getLines().takeWhile(_.nonEmpty), _.asDigit).withDefaultValue(9)

    val lowPoints = heightmap.filter {
      case (pos, value) => pos.directions.forall(p => heightmap(p) > value)
    }.toMap

    printDayPart(1, lowPoints.values.map(_ + 1).sum, "sum of the risk levels of all low points: %s")

    val basins = lowPoints.keys.map(getBasin(_, heightmap))
    val productOfLargestThree = basins.map(_.size).toVector.sorted.takeRight(3).reduceLeft(_ * _)

    printDayPart(2, productOfLargestThree, "product of largest 3 basins: %s")
  }

  private def getBasin(lowPoint: Pos, heightmap: Map[Pos, Int]): Set[Pos] = {
    @tailrec
    def grow(basin: Set[Pos], currentOutline: Set[Pos]): Set[Pos] = {
      val newOutline = currentOutline.flatMap(_.directions.filter(n => !basin.contains(n) && heightmap(n) != 9).toSet)
      if (newOutline.isEmpty)
        basin
      else
        grow(basin ++ newOutline, newOutline)
    }

    grow(Set(lowPoint), Set(lowPoint))
  }
}

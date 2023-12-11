package adventofcode.y2023

import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day11 extends Year2023 {
  override val day = 11

  override def runDay(input: BufferedSource): Unit = {
    val galaxies = Pos.parseMap(input.getLines(), _ == '#').filter(_._2).keySet.toList
    val emptyX = getEmptySpace(galaxies, _.x)
    val emptyY = getEmptySpace(galaxies, _.y)

    val expansion1 = expand(galaxies, emptyX, emptyY)
    printDayPart(1, getDinstances(expansion1).sum, "Sum of distances after initial expansion: %s")

    val expansion1Mil = expand(galaxies, emptyX, emptyY, 999999)
    printDayPart(2, getDinstances(expansion1Mil).sum, "Sum of distances after big expansion: %s")
  }
  private def getEmptySpace(universe: List[Pos], orientation: Pos => Int): List[Int] =
    (0 to orientation(universe.maxBy(orientation)))
      .filterNot(empty => universe.exists(orientation(_) == empty)).toList.reverse

  private def expand(galaxies: List[Pos], emptyX: List[Int], emptyY: List[Int], amount: Int = 1): List[Pos] = {
    val xExpanded = emptyX.foldLeft(galaxies) { (galaxies, x) =>
      galaxies.map(pos => if (pos.x > x) pos + (amount, 0) else pos)
    }
    emptyY.foldLeft(xExpanded) { (galaxies, y) =>
      galaxies.map(pos => if (pos.y > y) pos + (0, amount) else pos)
    }
  }

  private def getDinstances(galaxies: List[Pos]): List[Long] =
    galaxies.combinations(2).map { x =>
      (x: @unchecked) match {
        case List(a, b) => a.distance(b).toLong
      }
    }.toList
}

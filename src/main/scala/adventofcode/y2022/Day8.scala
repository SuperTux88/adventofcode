package adventofcode.y2022

import adventofcode.common.pos.{Direction, Pos}

import scala.io.BufferedSource

object Day8 extends Year2022 {
  override val day = 8

  override def runDay(input: BufferedSource): Unit = {
    val trees = Pos.parseMap(input.getLines().takeWhile(_.nonEmpty), _.asDigit)
    val (maxX, maxY) = (trees.maxBy(_._1.x)._1.x, trees.maxBy(_._1.y)._1.y)

    val visible =
      (0 to maxX).flatMap(x => getVisibleFromEdge(trees, Pos(x, 0), Direction.down)).toSet ++
      (0 to maxX).flatMap(x => getVisibleFromEdge(trees, Pos(x, maxY), Direction.up)).toSet ++
      (0 to maxY).flatMap(y => getVisibleFromEdge(trees, Pos(0, y), Direction.right)).toSet ++
      (0 to maxY).flatMap(y => getVisibleFromEdge(trees, Pos(maxX, y), Direction.left)).toSet

    printDayPart(1, visible.size, "Number of visible trees: %s")

    val scores = (1 until maxX).flatMap { x =>
      (1 until maxY).map(y => Direction.directions.map(getVisibleFromTree(trees, Pos(x, y), _)).product)
    }

    printDayPart(2, scores.max, "The highest score is: %s")
  }

  private def getVisibleFromEdge(trees: Map[Pos, Int], pos: Pos, direction: Pos): Set[Pos] =
    Iterator.iterate(pos)(_ + direction).takeWhile(trees.contains).foldLeft(-1, Set.empty[Pos]) {
      case ((maxHeight, visible), pos) =>
        val height = trees(pos)
        if (height > maxHeight) (height, visible + pos) else (maxHeight, visible)
    }._2

  private def getVisibleFromTree(trees: Map[Pos, Int], pos: Pos, direction: (Int, Int)): Int = {
    val height = trees(pos)
    val line = Iterator.iterate(pos)(_ + direction).takeWhile(trees.contains).drop(1).toList
    line.indexWhere(trees(_) >= height) match {
      case -1 => line.size
      case index => index + 1
    }
  }
}

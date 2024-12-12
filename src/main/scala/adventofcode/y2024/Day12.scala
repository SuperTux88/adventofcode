package adventofcode.y2024

import adventofcode.common.pos.{Direction, Pos}

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day12 extends Year2024 {
  override val day = 12

  override def runDay(input: BufferedSource): Unit = {
    val map = Pos.parseMap(input.getLines(), identity)

    printDayPart(1, calculateAllPrices(map, calculatePerimeter), "Total price: %s")
    printDayPart(2, calculateAllPrices(map, calculateSides), "Total price with bulk discount: %s")
  }

  private def calculateAllPrices(map: Map[Pos, Char], perimeterFn: Set[Pos] => Int): Int = {
    @tailrec
    def calculateAll(positions: Set[Pos], price: Int): Int = {
      if (positions.isEmpty) price
      else {
        val current = positions.head
        val (newArea, newPerimeter) = calculateOne(map(current), Set(current), Set(current))
        calculateAll(positions.diff(newArea), price + (newArea.size * newPerimeter))
      }
    }

    @tailrec
    def calculateOne(char: Char, current: Set[Pos], area: Set[Pos]): (Set[Pos], Int) = {
      val next = current.flatMap(_.directions.filter(map.get(_).contains(char)))
      if (next.isEmpty) (area, perimeterFn(area))
      else calculateOne(char, next.diff(area), area ++ next)
    }

    calculateAll(map.keySet, 0)
  }

  private def calculatePerimeter(area: Set[Pos]): Int =
    area.foldLeft(0) { (perimeter, p) => perimeter + p.directions.count(!area.contains(_)) }

  private def calculateSides(area: Set[Pos]): Int = {
    def filterDirection(direction: Int): Set[Pos] =
      area.filter(p => !area.contains(p.moveDirectionIndex(direction)))

    @tailrec
    def groupByNeighbours(remaining: Set[Pos], sides: Int, left: Int, right: Int): Int =
      if (remaining.isEmpty) sides
      else {
        val current = remaining.head
        val line = Iterator.iterate(current)(_.moveDirectionIndex(left)).takeWhile(remaining.contains).toSet
          ++ Iterator.iterate(current)(_.moveDirectionIndex(right)).takeWhile(remaining.contains).toSet
        groupByNeighbours(remaining.diff(line), sides + 1, left, right)
      }

    (0 to 3).map(dir =>
      groupByNeighbours(filterDirection(dir), 0, Direction.rotateLeft(dir), Direction.rotateRight(dir))
    ).sum
  }
}

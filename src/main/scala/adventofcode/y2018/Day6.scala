package adventofcode.y2018

import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day6 extends Year2018 {
  override val day = 6

  override def runDay(input: BufferedSource): Unit = {
    val coordinates = input.getLines().map { line =>
      val coordinate = line.split(", ").map(_.toInt)
      Pos(coordinate.head, coordinate.last)
    }.toList

    val min = Pos(coordinates.minBy(_.x).x, coordinates.minBy(_.y).y)
    val max = Pos(coordinates.maxBy(_.x).x, coordinates.maxBy(_.y).y)

    val map = Array.fill(max.x + 1, max.y + 1)(None: Option[Int])
    val coordsWithIndex = coordinates.zipWithIndex
    var regionSize = 0

    for (x <- min.x to max.x; y <- min.y to max.y) {
      var minimalDistance = Int.MaxValue
      var totalDistance = 0

      coordsWithIndex.foreach { coordinate =>
        val distance = coordinate._1.distance(Pos(x, y))
        if (distance <= minimalDistance) {
          map(x)(y) = if (distance < minimalDistance) Some(coordinate._2) else None
          minimalDistance = distance
        }
        totalDistance += distance
      }

      if (totalDistance < 10000) regionSize += 1
    }

    val border = (map.map(_ (min.y)) ++ map.map(_ (max.y)) ++ map(min.x) ++ map(max.x)).toSet
    val fieldsByNearestCoordinate = map.flatMap(_.toList).groupBy(identity)

    printDayPart(1, fieldsByNearestCoordinate.view.filterKeys(!border.contains(_)).values.map(_.length).max)
    printDayPart(2, regionSize)
  }
}

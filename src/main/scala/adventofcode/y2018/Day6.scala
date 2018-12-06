package adventofcode.y2018

object Day6 extends Year2018 {
  override val day = 6

  private val coordinates = input.getLines.map { line =>
    val coordinate = line.split(", ").map(_.toInt)
    Coord(coordinate.head, coordinate.last)
  }.toList

  private val min = Coord(coordinates.minBy(_.x).x, coordinates.minBy(_.y).y)
  private val max = Coord(coordinates.maxBy(_.x).x, coordinates.maxBy(_.y).y)

  private val map = Array.fill(max.x + 1, max.y + 1)(None:Option[Int])
  private val coordsWithIndex = coordinates.zipWithIndex
  private var regionSize = 0

  for (x <- min.x to max.x; y <- min.y to max.y) {
    var minimalDistance = Int.MaxValue
    var totalDistance = 0

    coordsWithIndex.foreach { coordinate =>
      val distance = coordinate._1.distance(Coord(x, y))
      if (distance <= minimalDistance) {
        map(x)(y) = if (distance < minimalDistance) Some(coordinate._2) else None
        minimalDistance = distance
      }
      totalDistance += distance
    }

    if (totalDistance < 10000) regionSize += 1
  }

  private val border = (map.map(_(min.y)) ++ map.map(_(max.y)) ++ map(min.x) ++ map(max.x)).toSet
  private val fieldsByNearestCoordinate = map.flatMap(_.toList).groupBy(identity)

  printDayPart(1, fieldsByNearestCoordinate.filterKeys(!border.contains(_)).values.map(_.length).max)
  printDayPart(2, regionSize)

  private case class Coord(x: Int, y: Int) {
    def distance(other: Coord): Int = (x - other.x).abs + (y - other.y).abs
  }
}

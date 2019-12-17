package adventofcode.y2019

import adventofcode.common.Pos
import adventofcode.common.NumberHelper.gcd

import scala.collection.parallel.CollectionConverters._

object Day10 extends Year2019 {
  override val day = 10

  private val asteroids = input.getLines.zipWithIndex.flatMap {
    case (line, y) =>
      line.zipWithIndex.flatMap {
        case ('#', x) => Some(Pos(x, y))
        case _ => None
      }
  }.toSet

  private val (stationPos, visibleAsteroids) = asteroids.par.map { asteroid =>
    asteroid -> getVisibleAsteroids(asteroid, asteroids - asteroid)
  }.maxBy(_._2.size)

  printDayPart(1, visibleAsteroids.size, "visible asteroids: %s")

  private val vaporizeOrder = getVaporizeOrder(stationPos, asteroids - stationPos)

  private val twohundredthAsteroid = vaporizeOrder(199)
  printDayPart(2, twohundredthAsteroid.x * 100 + twohundredthAsteroid.y)

  private def getVisibleAsteroids(asteroid: Pos, otherAsteroids: Set[Pos]) =
    otherAsteroids.filterNot { otherAsteroid =>
      val direction = otherAsteroid - asteroid
      val greatestCommonDivisor = gcd(direction.x, direction.y).toInt
      val minDirection = direction / greatestCommonDivisor

      (1 until greatestCommonDivisor).exists(mul => otherAsteroids.contains(asteroid + minDirection * mul))
    }

  import scala.math.Ordering.Double.TotalOrdering

  private def getVaporizeOrder(station: Pos, remaining: Set[Pos]): Seq[Pos] =
    if (remaining.nonEmpty) {
      val visible = getVisibleAsteroids(station, remaining)
      val orderedAsteroids = visible.toVector.sortBy(asteroid => {
        val direction = asteroid - station
        math.atan2(direction.x, direction.y) * -1
      })
      orderedAsteroids ++ getVaporizeOrder(station, remaining -- visible)
    } else {
      Nil
    }
}

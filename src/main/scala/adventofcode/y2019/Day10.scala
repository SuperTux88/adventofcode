package adventofcode.y2019

import adventofcode.common.NumberHelper.gcd
import adventofcode.common.pos.Pos

import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day10 extends Year2019 {
  override val day = 10

  override def runDay(input: BufferedSource): Unit = {
    val asteroids = Pos.parseSet(input.getLines())

    val (stationPos, visibleAsteroids) = asteroids.par.map { asteroid =>
      asteroid -> getVisibleAsteroids(asteroid, asteroids - asteroid)
    }.maxBy(_._2.size)

    printDayPart(1, visibleAsteroids.size, "visible asteroids: %s")

    val vaporizeOrder = getVaporizeOrder(stationPos, asteroids - stationPos)

    val twohundredthAsteroid = vaporizeOrder(199)
    printDayPart(2, twohundredthAsteroid.x * 100 + twohundredthAsteroid.y)
  }

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

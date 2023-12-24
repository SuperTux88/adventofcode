package adventofcode.y2023

import scala.io.BufferedSource

object Day24 extends Year2023 {
  override val day = 24

  private val HailstoneRE = """(\d+), (\d+), (\d+) @ +(-?\d+), +(-?\d+), +(-?\d+)""".r

  // private val FROM = 7L
  // private val TO = 27L
  private val FROM = 200000000000000L
  private val TO = 400000000000000L

  override def runDay(input: BufferedSource): Unit = {
    val hailstones = input.getLines().takeWhile(_.nonEmpty).map {
      case HailstoneRE(x, y, z, vx, vy, vz) =>
        Hailstone(Pos3DDouble(x.toDouble, y.toDouble, z.toDouble), Pos3DDouble(vx.toDouble, vy.toDouble, vz.toDouble))
    }.toList

    val intersections = hailstones.combinations(2).flatMap { comb =>
      (comb: @unchecked) match {
        case List(h1, h2) =>
          h1.intersectionWith2D(h2).map((h1, h2, _))
      }
    }.toList

    val valid = intersections.filter { (h1, h2, pos) =>
      isInField(pos) && h1.isInFuture2D(pos) && h2.isInFuture2D(pos)
    }

    printDayPart(1, valid.size, "Hailstones which will intersect on a 2D plane: %s")


    // TODO: printDayPart(2, 0, "Sum of coordinates of initial position: %s")
  }

  private def isInField(pos: PosDouble): Boolean =
    pos.x >= FROM && pos.x <= TO && pos.y >= FROM && pos.y <= TO

  private case class Hailstone(pos: Pos3DDouble, velocity: Pos3DDouble) {
    private val slope2D = velocity.y / velocity.x
    private val yOffset2D = pos.y - slope2D * pos.x

    def intersectionWith2D(other: Hailstone): Option[PosDouble] =
      if (slope2D == other.slope2D) None
      else {
        val x = (other.yOffset2D - yOffset2D) / (slope2D - other.slope2D)
        val y = slope2D * x + yOffset2D
        Some(PosDouble(x, y))
      }

    def isInFuture2D(other: PosDouble): Boolean =
      if (velocity.x == 0)
        (other.y - pos.y) / velocity.y > 0
      else
        (other.x - pos.x) / velocity.x > 0
  }

  private case class PosDouble(x: Double, y: Double)

  private case class Pos3DDouble(x: Double, y: Double, z: Double)
}

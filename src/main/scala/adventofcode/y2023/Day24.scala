package adventofcode.y2023

import scala.annotation.tailrec
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

    val combinations = hailstones.combinations(2).map { comb =>
      (comb: @unchecked) match {
        case List(h1, h2) => (h1, h2)
      }
    }.toList

    val intersections = combinations.flatMap { (h1, h2) =>
      h1.intersectionWith2D(h2).map((h1, h2, _))
    }

    val valid = intersections.filter { (h1, h2, pos) =>
      isInField(pos) && h1.isInFuture2D(pos) && h2.isInFuture2D(pos)
    }

    printDayPart(1, valid.size, "Hailstones which will intersect on a 2D plane: %s")

    val xVel = findPossibleVelocity(combinations, _.x)
    val yVel = findPossibleVelocity(combinations, _.y)
    val zVel = findPossibleVelocity(combinations, _.z)
    val stoneVelocity = Pos3DDouble(xVel.get.toDouble, yVel.get.toDouble, zVel.get.toDouble)

    val hail1 :: hail2 :: _ = hailstones: @unchecked

    val hail1RelativeVelocity = hail1.copy(velocity = getRelativeVelocity(stoneVelocity, hail1))
    val hail2RelativeVelocity = hail2.copy(velocity = getRelativeVelocity(stoneVelocity, hail2))

    val stoneStart2D = hail1RelativeVelocity.intersectionWith2D(hail2RelativeVelocity).get
    val time = (stoneStart2D.x - hail1.pos.x) / hail1RelativeVelocity.velocity.x
    val stoneStartZ = hail1.pos.z + time * hail1RelativeVelocity.velocity.z

    printDayPart(2, (stoneStart2D.x + stoneStart2D.y + stoneStartZ).toLong, "Sum of coordinates of initial position: %s")
  }

  private def getRelativeVelocity(stoneVelocity: Pos3DDouble, hail: Hailstone) =
    Pos3DDouble(hail.velocity.x - stoneVelocity.x, hail.velocity.y - stoneVelocity.y, hail.velocity.z - stoneVelocity.z)

  private def isInField(pos: PosDouble): Boolean =
    pos.x >= FROM && pos.x <= TO && pos.y >= FROM && pos.y <= TO

  @tailrec
  private def findPossibleVelocity(remaining: List[(Hailstone, Hailstone)], direction: Pos3DDouble => Double,
                                   possibilities: List[Int] = (-1000 to 1000).toList): Option[Int] =
    if (possibilities.size == 1) {
      Some(possibilities.head)
    } else {
      remaining match {
        case Nil => None
        case (h1, h2) :: tail =>
          val h1v = direction(h1.velocity)
          val h2v = direction(h2.velocity)
          if (h1v == h2v) {
            val diff = direction(h2.pos) - direction(h1.pos)
            val filteredList = possibilities.filter { v =>
              v != h1v && diff % (v - h1v) == 0
            }
            findPossibleVelocity(tail, direction, filteredList)
          } else {
            findPossibleVelocity(tail, direction, possibilities)
          }
      }
    }

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

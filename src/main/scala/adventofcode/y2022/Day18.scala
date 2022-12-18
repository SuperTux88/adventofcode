package adventofcode.y2022

import adventofcode.common.pos.Pos3D

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day18 extends Year2022 {
  override val day = 18

  private val CubeRE = """(\d+),(\d+),(\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val lava = input.getLines().map {
      case CubeRE(x, y, z) => Pos3D(x.toInt, y.toInt, z.toInt)
    }.toSet
    val airNeighbors = lava.flatMap(_.directions).diff(lava)

    val surface = lava.toSeq.map(_.directions.count(airNeighbors.contains)).sum
    printDayPart(1, surface, "Total surface area: %s")

    val bounds = Bounds(boundsDirection(lava, _.x), boundsDirection(lava, _.y), boundsDirection(lava, _.z))
    val outsideAir = airNeighbors.diff(findTrapped(airNeighbors, lava, bounds))
    val outsideSurface = lava.toSeq.map(_.directions.count(outsideAir.contains)).sum

    printDayPart(2, outsideSurface, "Total outside surface area: %s")
  }

  private def findTrapped(air: Set[Pos3D], lava: Set[Pos3D], bounds: Bounds): Set[Pos3D] = {
    @tailrec
    def findTraps(remaining: Set[Pos3D], outside: Set[Pos3D] = Set.empty, trapped: Set[Pos3D] = Set.empty): Set[Pos3D] = {
      @tailrec
      def canEscape(toCheck: Set[Pos3D], seen: Set[Pos3D] = Set.empty): (Boolean, Set[Pos3D]) = {
        if (toCheck.isEmpty) {
          (false, seen)
        } else {
          val next = toCheck.head
          val newSeen = seen + next

          if (outside.contains(next) || !bounds.contains(next)) {
            (true, newSeen)
          } else {
            canEscape((toCheck ++ next.directions).diff(lava).diff(newSeen), newSeen)
          }
        }
      }

      if (remaining.isEmpty) {
        trapped
      } else {
        canEscape(Set(remaining.head)) match {
          case (true, newOutside) => findTraps(remaining.diff(newOutside), outside ++ newOutside, trapped)
          case (false, newTrapped) => findTraps(remaining.diff(newTrapped), outside, trapped ++ newTrapped)
        }
      }
    }

    findTraps(air)
  }

  private def boundsDirection(lava: Set[Pos3D], direction: Pos3D => Int): (Int, Int) =
    (direction(lava.minBy(direction)), direction(lava.maxBy(direction)))

  private case class Bounds(x: (Int, Int), y: (Int, Int), z: (Int, Int)) {
    def contains(pos: Pos3D): Boolean =
      pos.x >= x._1 && pos.x <= x._2 && pos.y >= y._1 && pos.y <= y._2 && pos.z >= z._1 && pos.z <= z._2
  }
}

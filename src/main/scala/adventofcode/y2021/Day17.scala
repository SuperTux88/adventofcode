package adventofcode.y2021

import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

object Day17 extends Year2021 {
  override val day = 17

  private val AreaRE = """target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)""".r

  override def runDay(input: String): Unit = {
    val area = input match {
      case AreaRE(x1, x2, y1, y2) => Area(Pos(x1.toInt, y1.toInt), Pos(x2.toInt, y2.toInt))
    }

    val results: List[Int] = (math.sqrt(2 * area.xStart).toInt to area.xEnd).par.flatMap { x =>
      (area.yBottom to Seq(-area.yBottom, area.xEnd - x).min).flatMap { y =>
        fire(area, Pos.zero, (x, y))
      }
    }.toList

    printDayPart(1, results.max, "highest possible point: %s")
    printDayPart(2, results.size, "distinct velocities which hit the target: %s")
  }

  @tailrec
  private def fire(area: Area, pos: Pos, velocity: (Int, Int), highest: Int = 0): Option[Int] =
    if (area.isOvershot(pos))
      None
    else if (area.contains(pos))
      Some(highest)
    else
      fire(area, pos + velocity, (Seq(velocity._1 - 1, 0).max, velocity._2 - 1), Seq(pos.y, highest).max)

  private case class Area(area: (Pos, Pos)) {
    val Seq(xStart, xEnd) = Seq(area._1.x.toInt, area._2.x.toInt).sorted
    val Seq(yBottom, yTop) = Seq(area._1.y.toInt, area._2.y.toInt).sorted

    def contains(pos: Pos): Boolean = xStart <= pos.x && pos.x <= xEnd && yBottom <= pos.y && pos.y <= yTop
    def isOvershot(pos: Pos): Boolean = pos.x > xEnd || pos.y < yBottom
  }
}

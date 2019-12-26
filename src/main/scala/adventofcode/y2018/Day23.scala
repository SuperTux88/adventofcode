package adventofcode.y2018

import adventofcode.common.pos.Pos3D

import scala.collection.mutable

object Day23 extends Year2018 {
  override val day = 23

  private val NanoBotRE = """pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)""".r

  private val bots = input.getLines.map {
    case NanoBotRE(x, y, z, r) => Bot(Pos3D(x.toInt, y.toInt, z.toInt), r.toInt)
  }.toList

  private val strongestBot = bots.maxBy(_.range)

  printDayPart(1, bots.count(strongestBot.isInRange))

  private val center = Pos3D(0, 0, 0)

  printDayPart(2, findClosestWithMostBots(bots).distance(center))

  private trait Octahedron {
    def pos: Pos3D
    def range: Int

    def isInRange(other: Octahedron): Boolean = pos.distance(other.pos) <= range
  }

  private case class Bot(pos: Pos3D, range: Int) extends Octahedron

  private case class SearchBox(pos: Pos3D, range: Int) extends Octahedron {
    def overlaps(other: Octahedron): Boolean = pos.distance(other.pos) <= range + other.range

    def split: Set[SearchBox] = {
      val offset = (range / 3.0).ceil.toInt
      var offsets = Set(
        (-offset, 0, 0), (offset, 0, 0),
        (0, -offset, 0), (0, offset, 0),
        (0, 0, -offset), (0, 0, offset),
      )
      if (range == 1) offsets += ((0, 0, 0)) // include center for range 0
      offsets.map(o => SearchBox(pos + o, range - offset))
    }
  }

  // using modified A*
  private def findClosestWithMostBots(bots: Seq[Bot]): Pos3D = {
    val initialCenter = Pos3D(middle(bots.map(_.pos.x)), middle(bots.map(_.pos.y)), middle(bots.map(_.pos.z)))
    val initialBox = SearchBox(initialCenter, bots.map(_.pos.distance(initialCenter)).max)

    val Q = mutable.PriorityQueue(((bots.count(initialBox.overlaps), -initialBox.range), initialBox))(Ordering.by(_._1))
    val seen = mutable.Set[Octahedron]()

    while(Q.nonEmpty) {
      val (currentHeuristic, currentBox) = Q.dequeue()
      if (currentBox.range == 0) {
        val sameBotCountPositions = currentBox.pos :: Q.filter(_._1 == currentHeuristic).map(_._2.pos).toList
        return sameBotCountPositions.minBy(_.distance(center))
      } else if (seen.add(currentBox)) {
        currentBox.split.foreach { box =>
          Q.enqueue(((bots.count(box.overlaps), -box.range), box))
        }
      }
    }

    center // nothing found
  }

  private def middle(points: Iterable[Int]): Int = points.min + (points.max - points.min) / 2
}

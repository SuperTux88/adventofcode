package adventofcode.y2024

import adventofcode.Logging
import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day14 extends Year2024 {
  override val day = 14

  private val RobotRE = """p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""".r

  // private val SIZE = Pos(11, 7)
  private val SIZE = Pos(101, 103)

  private val QUADRANTS_BORDER = SIZE / 2
  private val QUADRANTS = QUADRANTS_BORDER + Pos(1, 1)

  private val OUTLIER_THRESHOLD = 30 // how many robots on the same row/col

  override def runDay(input: BufferedSource): Unit = {
    val robots = input.getLines().takeWhile(_.nonEmpty).map {
      case RobotRE(px, py, vx, vy) => Robot(Pos(px.toInt, py.toInt), Pos(vx.toInt, vy.toInt))
    }.toSeq

    val robots100 = (1 to 100).foldLeft(robots)((robots, _) => robots.map(_.move()))
    printMap(robots100)

    val quadrants = robots100.flatMap(_.quadrant).groupBy(identity).values.map(_.size)
    printDayPart(1, quadrants.product, "Safety factor after 100 seconds: %s")

    val (seconds, robotsTree) = findTree(robots)
    printMap(robotsTree)
    printDayPart(2, seconds, "Fewest numbers of seconds to display easter egg: %s")
  }

  private def findTree(robots: Seq[Robot]): (Int, Seq[Robot]) = {
    @tailrec
    def findOutlier(robots: Seq[Robot], seconds: Int = 1): (Int, Seq[Robot]) = {
      val newRobots = robots.map(_.move())
      if (newRobots.groupBy(_.p.x).values.exists(_.size > OUTLIER_THRESHOLD))
        (seconds, newRobots)
      else
        findOutlier(newRobots, seconds + 1)
    }

    @tailrec
    def search(robots: Seq[Robot], seconds: Int): (Int, Seq[Robot]) = {
      val newRobots = robots.map(_.move(SIZE.x))
      if (newRobots.groupBy(_.p).values.exists(_.size > 1))
        search(newRobots, seconds + SIZE.x)
      else
        (seconds + SIZE.x, newRobots)
    }

    val (seconds, outlierRobots) = findOutlier(robots)
    search(outlierRobots, seconds)
  }

  private case class Robot(p: Pos, v: Pos) {
    def move(steps: Int = 1): Robot = this.copy(p = (p + v * steps) %+ SIZE)
    def quadrant: Option[Pos] =
      if (p.x == QUADRANTS_BORDER.x || p.y == QUADRANTS_BORDER.y) None
      else Some(p / QUADRANTS)
  }

  private def printMap(robots: Seq[Robot]): Unit = if (Logging.debug)
    (0 until SIZE.y).foreach { y =>
      (0 until SIZE.x).foreach { x =>
        val pos = Pos(x, y)
        val posRobots = robots.count(_.p == pos)
        if (posRobots == 0) print(".")
        else print(posRobots)
      }
      println()
    }
}

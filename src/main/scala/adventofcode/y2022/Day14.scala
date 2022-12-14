package adventofcode.y2022

import adventofcode.Logging
import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day14 extends Year2022 {
  override val day = 14

  private val SOURCE = Pos(500, 0)
  private val FALLING_DIRECTIONS: List[Pos => Pos] = List(_.down, _.down.left, _.down.right)

  override def runDay(input: BufferedSource): Unit = {
    implicit val rock: Set[Pos] = input.getLines().flatMap {
      _.split(" -> ").map {
        case Pos.PointRE(x, y) => Pos(x.toInt, y.toInt)
      }.sliding(2).flatMap(p => p(0).lineTo(p(1)))
    }.toSet

    val maxDepth = rock.maxBy(_.y).y
    implicit val floor: Int = maxDepth + 2

    val sand = simulate(_.y > maxDepth)
    printMap(rock, sand)
    printDayPart(1, sand.size, "Units of sand before falling into the abyss: %s")

    val sandWithFloor = simulate(_ == SOURCE, sand) + SOURCE
    printMap(rock, sandWithFloor)
    printDayPart(2, sandWithFloor.size, "Units of sand with a floor: %s")
  }

  @tailrec
  private def simulate(endCondition: Pos => Boolean, sand: Set[Pos] = Set.empty)(implicit rock: Set[Pos], floor: Int): Set[Pos] = {
    def isFree(pos: Pos) = !rock.contains(pos) && !sand.contains(pos) && pos.y < floor

    @tailrec
    def fall(pos: Pos): Pos = FALLING_DIRECTIONS.map(_(pos)).find(fallen => isFree(fallen)) match {
      case Some(fallen) => fall(fallen)
      case None => pos
    }

    val fallen = fall(SOURCE)
    if (endCondition(fallen)) sand
    else simulate(endCondition, sand + fallen)
  }

  def printMap(rock: Set[Pos], sand: Set[Pos]): Unit = if (Logging.debug) {
    val floor = rock.maxBy(_.y).y + 2
    (0 to sand.maxBy(_.y).y + 1).foreach(y =>
      println((rock.minBy(_.x).x.min(sand.minBy(_.x).x) to rock.maxBy(_.x).x.max(sand.maxBy(_.x).x)).map { x =>
        if (rock.contains(Pos(x, y)) || y == floor) "#"
        else if (sand.contains(Pos(x, y))) "o"
        else " "
      }.mkString)
    )
  }
}

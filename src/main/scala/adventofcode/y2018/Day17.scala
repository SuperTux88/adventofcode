package adventofcode.y2018

import adventofcode.Logging
import adventofcode.common.pos.Pos

object Day17 extends Year2018 {
  override val day = 17

  val ClayVerticalRE = """x=(\d+), y=(\d+)\.\.(\d+)""".r
  val ClayHorizontalRE = """y=(\d+), x=(\d+)\.\.(\d+)""".r

  private val clay = input.getLines.flatMap {
    case ClayVerticalRE(x, yFrom, yTo) => (yFrom.toInt to yTo.toInt).map(Pos(x.toInt, _))
    case ClayHorizontalRE(y, xFrom, xTo) => (xFrom.toInt to xTo.toInt).map(Pos(_, y.toInt))
  }.toSet
  private val maxDepth = clay.maxBy(_.y).y

  private val water = flow(Pos(500, clay.minBy(_.y).y))
  printMap(water)

  printDayPart(1, water.size)
  printDayPart(2, water.count(_._2))

  private def flow(pos: Pos, water: Map[Pos, Boolean] = Map[Pos, Boolean]()): Map[Pos, Boolean] = {
    def free(pos: Pos) = !clay.contains(pos) && !water.getOrElse(pos, false)

    def iterate(pos: Pos, line: Map[Pos, Boolean] = Map[Pos, Boolean]())(direction: Pos => Pos) =
      Iterator.iterate(pos)(direction).takeWhile(p => free(p) && !free(p.down))
        .foldLeft(line, pos) { (state, currentPos) =>
          (state._1 + (currentPos -> false), direction(currentPos))
        }

    if (pos.y <= maxDepth && free(pos)) {
      if (!free(pos.down)) { // fill up and overflow
        val (leftLine, left) = iterate(pos)(_.left)
        val (line, right) = iterate(pos, leftLine)(_.right)

        if (free(left.down) || free(right.down)) {
          flow(left, flow(right, water ++ line)) // overflow
        } else {
          water ++ line.view.mapValues(_ => true) // fill line
        }
      } else if (!water.contains(pos)) { // flow downward
        val flowDown = flow(pos.down, water + (pos -> false))
        if (flowDown.getOrElse(pos.down, false)) {
          flow(pos, flowDown) // fill up and overflow
        } else flowDown
      } else water
    } else water
  }

  private def printMap(water: Map[Pos, Boolean]): Unit = if (Logging.debug) {
    val xRange = (water.keys ++ clay).minBy(_.x).x to (water.keys ++ clay).maxBy(_.x).x
    (clay.minBy(_.y).y to maxDepth).foreach { y =>
      println(xRange.map { x =>
        if (clay.contains(Pos(x, y))) '#'
        else if (water.getOrElse(Pos(x, y), false)) '~'
        else if (water.contains(Pos(x, y))) '|'
        else '.'
      }.mkString)
    }
  }
}

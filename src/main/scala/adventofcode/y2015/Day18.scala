package adventofcode.y2015

import adventofcode.common.pos.Pos

object Day18 extends Year2015 {
  override val day: Int = 18

  val initialGrid = input.getLines().zipWithIndex.flatMap {
    case (line, x) => line.zipWithIndex.map {
      case ('#', y) => Pos(x, y) -> true
      case ('.', y) => Pos(x, y) -> false
      case (c, _) => throw new MatchError(s"Invalid character $c")
    }
  }.toMap.withDefaultValue(false)

  val gridAfter100Steps = (1 to 100).foldLeft(initialGrid) {
    (currentGrid, _) => nextGrid(currentGrid)
  }
  printDayPart(1, gridAfter100Steps.values.count(identity), "total lights on after 100 steps: %s")

  val gridWithStuckLightsAfter100Steps = (1 to 100).foldLeft(withStuckLights(initialGrid)) {
    (currentGrid, _) => withStuckLights(nextGrid(currentGrid))
  }
  printDayPart(2, gridWithStuckLightsAfter100Steps.values.count(identity), "total lights on with stuck lights: %s")

  private def nextState(on: Boolean, neighbors: Int) = (on, neighbors) match {
    case (true, 2) | (true, 3) | (false, 3) => true
    case _ => false
  }

  private def nextGrid(currentGrid: Map[Pos, Boolean]): Map[Pos, Boolean] =
    (for {
      x <- 0 to 99
      y <- 0 to 99
    } yield Pos(x, y))
      .map { p =>
        p -> nextState(currentGrid(p), p.neighborsWithDiagonals.count(currentGrid))
      }.toMap.withDefaultValue(false)

  private def withStuckLights(grid: Map[Pos, Boolean]) =
    grid + (Pos(0, 0) -> true) + (Pos(0, 99) -> true) + (Pos(99, 99) -> true) + (Pos(99, 0) -> true)
}

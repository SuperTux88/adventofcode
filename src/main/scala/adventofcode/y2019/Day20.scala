package adventofcode.y2019

import adventofcode.common.MapImplicits.MapImplicits
import adventofcode.common.pos.Pos
import adventofcode.common.search.Dijkstra

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day20 extends Year2019 {
  override val day = 20

  override def runDay(input: BufferedSource): Unit = {
    val map = Pos.parseMap(input.getLines(), char => char).withDefaultValue(' ')

    val labels = map.foldLeft(Map.empty[String, List[Label]].withDefaultValue(List.empty)) {
      case (labels, (pos, char)) if char.isLetter =>
        val downChar = map(pos.down)
        if (downChar.isLetter) {
          val label = if (map(pos.up) == '.')
            Label(s"$char$downChar", pos.up, pos)
          else
            Label(s"$char$downChar", pos.down.down, pos.down)
          labels + (label.name -> (label :: labels(label.name)))
        } else {
          val rightChar = map(pos.right)
          if (rightChar.isLetter) {
            val label = if (map(pos.left) == '.')
              Label(s"$char$rightChar", pos.left, pos)
            else
              Label(s"$char$rightChar", pos.right.right, pos.right)
            labels + (label.name -> (label :: labels(label.name)))
          } else {
            labels
          }
        }
      case (labels, _) => labels
    }

    val start = labels("AA").head
    val end = labels("ZZ").head
    val portals = labels.foldLeft(Map.empty[Pos, Label]) {
      case (portals, (_, a :: b :: Nil)) => portals ++ Map(a.labelPos -> b, b.labelPos -> a)
      case (portals, _) => portals
    } + (start.labelPos -> start) // start needed in portals to create start state

    val maze = Maze(map, portals, start, end)

    val minimalStepsToExit = distanceToExit((state: State) => state.pos == end.labelPos)(maze)
    printDayPart(1, minimalStepsToExit, "minimal steps to exit: %s")

    val minimalStepsToExitWithLevels = distanceToExit((state: State) => state == State(end.labelPos))(maze)
    printDayPart(2, minimalStepsToExitWithLevels, "minimal steps to exit with recursion: %s")
  }

  private def distanceToExit(isExit: State => Boolean)(implicit maze: Maze) =
    Dijkstra(
      State(maze.start.labelPos),
      isExit,
      (state: State) =>
        maze.portals.get(state.pos).toList.flatMap(target =>
          walkToLabels(List(target.pos), Set(target.labelPos)).map {
            case (dist, pos) => (dist, State(pos, state.level + levelDiff(pos)))
          }
        ).filter(_._2.level >= 0) // don't go further out than outermost level
    )._1 - 1

  @tailrec
  private def walkToLabels(positions: List[Pos], visited: Set[Pos], labels: List[(Int, Pos)] = List.empty, steps: Int = 1)(implicit maze: Maze): List[(Int, Pos)] = {
    if (positions.isEmpty) {
      labels
    } else {
      val nextSteps = positions.flatMap { pos =>
        pos.directions.filterNot(pos => visited.contains(pos) || maze.map(pos) == '#')
      }

      val (foundLabels, newPositions) = nextSteps.partition(pos => maze.map(pos).isLetter)
      walkToLabels(newPositions, visited ++ positions, foundLabels.map((steps, _)) ::: labels, steps + 1)
    }
  }

  private def levelDiff(pos: Pos)(implicit maze: Maze) =
    if (pos == maze.start.labelPos || pos == maze.end.labelPos) 0 // don't change levels for start and end
    else
      if (maze.topLeft.x <= pos.x && maze.topLeft.y <= pos.y &&
        pos.x <= maze.bottomRight.x && pos.y <= maze.bottomRight.y) 1 else -1

  private case class Maze(map: Map[Pos, Char], portals: Map[Pos, Label], start: Label, end: Label) {
    private val walls = map.keySetByValue('#')
    val (topLeft, bottomRight) = (Pos(2, 2), Pos(walls.map(_.x).max, walls.map(_.y).max))
  }

  private case class Label(name: String, pos: Pos, labelPos: Pos)
  private case class State(pos: Pos, level: Int = 0)
}

package adventofcode.y2019

import adventofcode.common.pos.Pos
import adventofcode.common.search.Dijkstra

import scala.annotation.tailrec

object Day20 extends Year2019 {
  override val day = 20

  private val map = Pos.parseMap(input.getLines(), char => char).withDefaultValue(' ')

  private val labels = map.foldLeft(Map.empty[String, List[Label]].withDefaultValue(List.empty)) {
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

  private val walls = map.filter(_._2 == '#').keys.toSet
  private val (topLeft, bottomRight) = (Pos(2, 2), Pos(walls.map(_.x).max, walls.map(_.y).max))

  private val start = labels("AA").head
  private val end = labels("ZZ").head
  private val portals = labels.foldLeft(Map.empty[Pos, Label]) {
    case (portals, (_, a :: b :: Nil)) => portals ++ Map(a.labelPos -> b, b.labelPos -> a)
    case (portals, _) => portals
  } + (start.labelPos -> start) // start needed in portals to create start state

  private val minimalStepsToExit = distanceToExit((state: State) => state.pos == end.labelPos)
  printDayPart(1, minimalStepsToExit, "minimal steps to exit: %s")

  private val minimalStepsToExitWithLevels = distanceToExit((state: State) => state == State(end.labelPos))
  printDayPart(2, minimalStepsToExitWithLevels, "minimal steps to exit with recursion: %s")

  private def distanceToExit(isExit: State => Boolean) =
    Dijkstra(
      State(start.labelPos),
      isExit,
      (state: State) =>
        portals.get(state.pos).toList.flatMap(target =>
          walkToLabels(List(target.pos), Set(target.labelPos)).map {
            case (dist, pos) => (dist, State(pos, state.level + levelDiff(pos)))
          }
        ).filter(_._2.level >= 0) // don't go further out than outermost level
    )._1 - 1

  @tailrec
  private def walkToLabels(positions: List[Pos], visited: Set[Pos], labels: List[(Int, Pos)] = List.empty, steps: Int = 1): List[(Int, Pos)] = {
    if (positions.isEmpty) {
      labels
    } else {
      val nextSteps = positions.flatMap { pos =>
        pos.directions.filterNot(pos => visited.contains(pos) || map(pos) == '#')
      }

      val (foundLabels, newPositions) = nextSteps.partition(pos => map(pos).isLetter)
      walkToLabels(newPositions, visited ++ positions, foundLabels.map((steps, _)) ::: labels, steps + 1)
    }
  }

  private def levelDiff(pos: Pos) =
    if (pos == start.labelPos || pos == end.labelPos) 0 // don't change levels for start and end
    else if (topLeft.x <= pos.x && topLeft.y <= pos.y && pos.x <= bottomRight.x && pos.y <= bottomRight.y) 1 else -1

  case class Label(name: String, pos: Pos, labelPos: Pos)
  case class State(pos: Pos, level: Int = 0)
}

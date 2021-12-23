package adventofcode.y2021

import adventofcode.common.NumberHelper.rangeWithReverse
import adventofcode.common.pos.Pos
import adventofcode.common.search.Dijkstra

import scala.io.BufferedSource

object Day23 extends Year2021 {
  override val day = 23

  private val COSTS = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
  private val FOLDED = Map(
    Pos(3, 3) -> 'D', Pos(5, 3) -> 'C', Pos(7, 3) -> 'B', Pos(9, 3) -> 'A',
    Pos(3, 4) -> 'D', Pos(5, 4) -> 'B', Pos(7, 4) -> 'A', Pos(9, 4) -> 'C'
  )

  private val HALLWAY_POSITIONS = Set(Pos(1, 1), Pos(2, 1), Pos(4, 1), Pos(6, 1), Pos(8, 1), Pos(10, 1), Pos(11, 1))
  private val FIRST_ROW = 2
  private val COLS = COSTS.keys.zip(List(3, 5, 7, 9)).toMap

  override def runDay(input: BufferedSource): Unit = {
    val startPositions = Pos.parseMap(input.getLines().takeWhile(_.nonEmpty), identity)
      .filter(pos => COSTS.keySet.contains(pos._2))

    printDayPart(1, getLowestEnergy(startPositions), "least energy required to organize the amphipods: %s")

    val unfolded = startPositions.filter(_._1.y == FIRST_ROW) ++ FOLDED
      ++ startPositions.filterNot(_._1.y == FIRST_ROW).map { case (p, a) => p + (0, 2) -> a }
    printDayPart(2, getLowestEnergy(unfolded), "least energy required to organize the amphipods after unfold: %s")
  }

  private def getLowestEnergy(startPositions: Map[Pos, Char]): Int = {
    val rows = 2 to startPositions.map(_._1.y).max
    val goal = COLS.flatMap { case (a, x) => rows.map(y => Pos(x, y) -> a) }
    Dijkstra(
      startPositions,
      state => state == goal,
      state => getMoveOutStates(state, rows) ::: getMoveInStates(state, rows)
    )._1
  }

  private def getMoveOutStates(state: Map[Pos, Char], rows: Range): List[(Int, Map[Pos, Char])] =
    state.filter { case (pos, amphipod) =>
      !HALLWAY_POSITIONS.contains(pos)
        && freeAbove(state, rows.start, pos)
        && !completedBelow(state, rows.end, pos, amphipod)
    }.toList.flatMap { case (from, amphipod) => HALLWAY_POSITIONS
      .filterNot(target => rangeWithReverse(from.x, target.x).exists(x => state.contains(Pos(x, target.y))))
      .map(target => (from.distance(target) * COSTS(amphipod), state.removed(from).updated(target, amphipod)))
    }

  private def getMoveInStates(state: Map[Pos, Char], rows: Range): List[(Int, Map[Pos, Char])] =
    state.filter(pos => HALLWAY_POSITIONS.contains(pos._1)).toList.flatMap { case (from, amphipod) =>
      rows.map(Pos(COLS(amphipod), _)).find { target =>
        freeAbove(state, rows.start, target) && completedBelow(state, rows.end, target, amphipod)
          && !rangeWithReverse(target.x, from.x, false).exists(x => state.contains(Pos(x, from.y)))
      }.map(target => (from.distance(target) * COSTS(amphipod), state.removed(from).updated(target, amphipod)))
    }

  private def freeAbove(state: Map[Pos, Char], firstRow: Int, pos: Pos) =
    !(firstRow until pos.y).exists(y => state.contains(Pos(pos.x, y)))

  private def completedBelow(state: Map[Pos, Char], lastRow: Int, pos: Pos, amphipod: Char) =
    COLS(amphipod) == pos.x && (pos.y + 1 to lastRow).forall(y => state.get(Pos(pos.x, y)).contains(amphipod))
}

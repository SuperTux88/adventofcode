package adventofcode.y2022

import adventofcode.common.NumberHelper
import adventofcode.common.pos.Pos
import adventofcode.common.search.AStar

import scala.io.BufferedSource

object Day24 extends Year2022 {
  override val day = 24

  private val FREE = '.'
  private val WALL = '#'
  private val BLIZZARDS = Set('^', '>', 'v', '<')

  override def runDay(input: BufferedSource): Unit = {
    val initialState = Pos.parseMap(input.getLines(), identity)
    val maxPos = initialState.keySet.max
    val start = initialState.collectFirst { case (pos, FREE) if pos.y == 0 => pos }.get
    val end = initialState.collectFirst { case (pos, FREE) if pos.y == maxPos.y => pos }.get

    val walls = initialState.collect { case (pos, WALL) => pos }.toSet + start.up + end.down
    val blizzards = initialState.collect { case (pos, dir) if BLIZZARDS.contains(dir) => Blizzard(pos, dir) }

    val blizzardLoop = NumberHelper.lcm(maxPos.x -1, maxPos.y -1).toInt
    val blockedAtTimes = (1 until blizzardLoop).scanLeft(blizzards) { case (blizzards, _) =>
      blizzards.map(_.move(maxPos))
    }.map(_.map(_.pos).toSet ++ walls)

    val (duration, path) = simulate(State(start), end, getNeighbors(blockedAtTimes))
    printDayPart(1, duration, "Time to reach the goal: %s minutes")

    val (durationBack, pathBack) = simulate(path.head, start, getNeighbors(blockedAtTimes))
    val (durationWithSnack, _) = simulate(pathBack.head, end, getNeighbors(blockedAtTimes))

    printDayPart(2, duration + durationBack + durationWithSnack,
      "Total time to reach the goal including getting the snack: %s minutes")
  }

  private def simulate(start: State, target: Pos, neighbors: (State, Pos) => List[(Int, Int, State)]): (Int, List[State]) =
    AStar(start, _.pos == target, neighbors(_, target))

  private def getNeighbors(blockedAtTimes: Seq[Set[Pos]])(state: State, target: Pos) =
    state.next(blockedAtTimes((state.minute + 1) % blockedAtTimes.size)).map(s => (1, s.pos.distance(target), s))

  private case class Blizzard(pos: Pos, direction: Char) {
    def move(maxPos: Pos): Blizzard = direction match {
      case '^' => if pos.y == 1 then copy(pos = Pos(pos.x, maxPos.y - 1)) else copy(pos = pos.up)
      case '>' => if pos.x == maxPos.x - 1 then copy(pos = Pos(1, pos.y)) else copy(pos = pos.right)
      case 'v' => if pos.y == maxPos.y - 1 then copy(pos = Pos(pos.x, 1)) else copy(pos = pos.down)
      case '<' => if pos.x == 1 then copy(pos = Pos(maxPos.x - 1, pos.y)) else copy(pos = pos.left)
    }
  }

  private case class State(pos: Pos, minute: Int = 0) {
    def next(blocked: Set[Pos]): List[State] =
      (pos :: pos.directions).filter(!blocked.contains(_)).map(p => State(p, minute + 1))
  }
}

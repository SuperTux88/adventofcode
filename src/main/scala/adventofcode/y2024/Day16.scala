package adventofcode.y2024

import adventofcode.Logging
import adventofcode.common.pos.{Direction, Pos}
import adventofcode.common.search.Dijkstra

import scala.collection.mutable
import scala.io.BufferedSource

object Day16 extends Year2024 {
  override val day = 16

  override def runDay(input: BufferedSource): Unit = {
    val (map, start, end) = Pos.parseMapStartEnd(input.getLines().takeWhile(_.nonEmpty))
    val startState = State(start, Direction.rightIndex)

    val shortestPath = Dijkstra(startState, _.pos == end, _.next(map))
    val bestScore = shortestPath._1
    printDayPart(1, bestScore, "Lowest possible score: %s")

    val bestRoutes = findAllBestRoutes(map, startState, end, bestScore)
    printMap(map, bestRoutes)
    printDayPart(2, bestRoutes.size, "Number of tiles which are part of at least one best path: %s")
  }

  private def findAllBestRoutes(map: Map[Pos, Char], start: State, end: Pos, score: Int): Set[Pos] = {
    def findAll(start: State, isTarget: State => Boolean, neighbors: State => List[(Int, State)]): Set[Pos] = {
      val Q = mutable.PriorityQueue((0, List(start)))(Ordering.by(-_._1)) // order by cost
      val seen = mutable.Map[State, Int]()
      val corners = mutable.Map[State, Set[Pos]]().withDefaultValue(Set.empty)

      while (Q.nonEmpty) {
        val (cost, current) = Q.dequeue()
        if (isTarget(current.head)) {
          return current.foldLeft(current.map(_.pos).toSet)((set, state) => set ++ corners(state))
        } else if (!seen.get(current.head).exists(_ < cost)) {
          seen(current.head) = cost
          neighbors(current.head).foreach { (nextCost, nextState) =>
            val newCost = cost + nextCost
            if (nextCost == 1001) corners(current.head) ++= current.map(_.pos).toSet
            if (newCost <= score) Q.enqueue((newCost, nextState :: current))
          }
        }
      }

      Set.empty
    }

    findAll(start, _.pos == end, _.next(map))
  }

  private case class State(pos: Pos, direction: Int) {
    def next(map: Map[Pos, Char]): List[(Int, State)] = List(
      move(map),
      rotate(map, Direction.rotateRight),
      rotate(map, Direction.rotateLeft)
    ).flatten

    private def move(map: Map[Pos, Char]): Option[(Int, State)] = {
      val nextPos = pos.moveDirectionIndex(direction)
      if (map.get(nextPos).contains('#')) None else Some((1, copy(pos = nextPos)))
    }

    private def rotate(map: Map[Pos, Char], rotate: Int => Int): Option[(Int, State)] = {
      val nextDirection = rotate(direction)
      val nextPos = pos.moveDirectionIndex(nextDirection)
      if (map.get(nextPos).contains('#')) None else Some((1001, State(nextPos, nextDirection)))
    }
  }

  private def printMap(map: Map[Pos, Char], path: Set[Pos]): Unit = if (Logging.debug)
    Pos.printMap(path.foldLeft(map)((m, pos) => m.updated(pos, 'O')), identity)
}

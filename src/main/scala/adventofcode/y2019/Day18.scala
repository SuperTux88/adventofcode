package adventofcode.y2019

import adventofcode.common.MapImplicits.MapImplicits
import adventofcode.common.pos.Pos
import adventofcode.common.search.Dijkstra

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.io.BufferedSource

object Day18 extends Year2019 {
  override val day = 18

  override def runDay(input: BufferedSource): Unit = {
    val map = Pos.parseMap(input.getLines(), char => char)

    val allKeys = map.values.filter(_.isLower).toSet
    val startPos = map.findKeyByValue('@').get

    val futurePart1 = Future {
      getShortestPath(Seq(startPos), map, allKeys)
    }

    val map2 = map + (startPos -> '#') ++ startPos.directions.map(_ -> '#')

    val futurePart2 = Future {
      getShortestPath(startPos.diagonals, map2, allKeys)
    }

    printDayPart(1, Await.result(futurePart1, Duration.Inf), "shortest path to all keys: %s")
    printDayPart(2, Await.result(futurePart2, Duration.Inf), "shortest path with robots: %s")
  }

  private def getShortestPath(startPositions: Seq[Pos], map: Map[Pos, Char], allKeys: Set[Char]): Int = {
    Dijkstra(
      State(startPositions, allKeys, Set.empty),
      (state: State) => state.remainingKeys.isEmpty,
      (state: State) =>
        state.positions.zipWithIndex.flatMap {
          case (pos, i) =>
            findNextKeys(List(pos), map, state.openDoors).map {
              case (dist, pos) =>
                val key = map(pos)
                (dist, State(state.positions.updated(i, pos), state.remainingKeys - key, state.openDoors + key.toUpper))
            }
        }.toList
    )._1
  }

  @tailrec
  private def findNextKeys(positions: List[Pos], map: Map[Pos, Char], openDoors: Set[Char], visited: Set[Pos] = Set.empty, foundKeys: List[(Int, Pos)] = List.empty, steps: Int = 1): List[(Int, Pos)] = {
    if (positions.isEmpty) {
      foundKeys
    } else {
      val nextSteps = positions.flatMap { pos =>
        pos.directions.filterNot(visited.contains).filter { pos =>
          map(pos) match {
            case '#' => false
            case door if door.isUpper => openDoors.contains(door)
            case _ => true
          }
        }
      }

      val (newPositions, foundKeyPos) = nextSteps.partition { pos =>
        map(pos) match {
          case key if key.isLower => openDoors.contains(key.toUpper)
          case _ => true
        }
      }

      findNextKeys(newPositions, map, openDoors, visited ++ nextSteps, foundKeyPos.map((steps, _)) ::: foundKeys, steps + 1)
    }
  }

  case class State(positions: Seq[Pos], remainingKeys: Set[Char], openDoors: Set[Char])
}

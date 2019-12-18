package adventofcode.y2019

import adventofcode.common.{Dijkstra, Pos}

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Day18 extends Year2019 {
  override val day = 18

  private val map = input.getLines().zipWithIndex.flatMap {
    case (line, y) => line.zipWithIndex.map {
      case (char, x) => Pos(x, y) -> char
    }
  }.toMap

  private val allKeys = map.values.filter(key => key >= 'a' && key <= 'z').toSet
  private val startPos = map.find(_._2 == '@').get._1

  private val futurePart1 = Future { getShortestPath(Seq(startPos), map) }

  private val map2 = map + (startPos -> '#') ++ Pos.directions.map(startPos + _).map(_ -> '#')
  private val startPositionsRobots = List((-1, -1), (-1, 1), (1, 1), (1, -1)).map(startPos + _)

  private val futurePart2 = Future { getShortestPath(startPositionsRobots, map2) }

  printDayPart(1, Await.result(futurePart1, Duration.Inf), "shortest path to all keys: %s")
  printDayPart(2, Await.result(futurePart2, Duration.Inf), "shortest path with robots: %s")

  private def getShortestPath(startPositions: Seq[Pos], map: Map[Pos, Char]): Int = {
    Dijkstra(
      State(startPositions, allKeys, Set.empty),
      { state: State => state.remainingKeys.isEmpty },
      { state: State =>
        state.positions.zipWithIndex.flatMap {
          case (pos, i) =>
            val reachableKeys = findNextKeys(List(pos), map, state.openDoors)
            reachableKeys.map {
              case (dist, pos) =>
                val key = map(pos)
                (dist, State(state.positions.updated(i, pos), state.remainingKeys - key, state.openDoors + key.toUpper))
            }
        }.toList
      }
    )._1
  }

  @tailrec
  private def findNextKeys(positions: List[Pos], map: Map[Pos, Char], openDoors: Set[Char], visited: Set[Pos] = Set.empty, foundKeys: List[(Int, Pos)] = List.empty, steps: Int = 1): List[(Int, Pos)] = {
    if (positions.isEmpty) {
      foundKeys
    } else {
      val nextSteps = positions.flatMap { pos =>
        Pos.directions.map(pos + _).filterNot(visited.contains).filter { pos =>
          map(pos) match {
            case '#' => false
            case door if door >= 'A' && door <= 'Z' => openDoors.contains(door)
            case _ => true
          }
        }
      }

      val (newPositions, foundKeyPos) = nextSteps.partition { pos =>
        map(pos) match {
          case key if key >= 'a' && key <= 'z' => openDoors.contains(key.toLower)
          case _ => true
        }
      }

      findNextKeys(newPositions, map, openDoors, visited ++ nextSteps, foundKeyPos.map((steps, _)) ::: foundKeys, steps + 1)
    }
  }

  case class State(positions: Seq[Pos], remainingKeys: Set[Char], openDoors: Set[Char])
}

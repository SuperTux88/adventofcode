package adventofcode.y2019

import adventofcode.common.{Dijkstra, Pos}

import scala.annotation.tailrec

object Day18 extends Year2019 {
  override val day = 18

  private val map = input.getLines().zipWithIndex.flatMap {
    case (line, y) => line.zipWithIndex.map {
      case (char, x) => Pos(x, y) -> char
    }
  }.toMap
  printMap(map)

  private val allKeys = map.values.filter(key => key >= 'a' && key <= 'z').toSet
  private val startPos = map.find(_._2 == '@').get._1

  private val shortestPath = Dijkstra(
    State(startPos, allKeys, Set.empty),
    { state: State => state.remainingKeys.isEmpty },
    { state: State =>
      val reachableKeys = findNextKeys(List(state.pos), map, state.openDoors)
      reachableKeys.map {
        case (dist, pos) =>
          val key = map(pos)
          (dist, State(pos, state.remainingKeys - key, state.openDoors + key.toUpper))
      }
    }
  )._1

  printDayPart(1, shortestPath, "shortest path to all keys: %s")

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

  private def printMap(map: Map[Pos, Char]): Unit =
    (0 to map.keys.map(_.y).max).foreach(y =>
      println((0 to map.keys.map(_.x).max).map(x => map(Pos(x, y))).mkString)
    )

  case class State(pos: Pos, remainingKeys: Set[Char], openDoors: Set[Char])
}

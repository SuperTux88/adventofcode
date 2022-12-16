package adventofcode.y2022

import adventofcode.common.search.{AStar, Dijkstra}

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day16 extends Year2022 {
  override val day = 16

  private val ValveRE = """Valve (\w{2}) has flow rate=(\d+); tunnels? leads? to valves? ([\w, ]+)""".r

  private val START = "AA"

  override def runDay(input: BufferedSource): Unit = {
    val valves = input.getLines().map {
      case ValveRE(name, flow, tunnels) => name -> Valve(flow.toInt, tunnels.split(", ").toList)
    }.toMap

    val valvesWithFlow = valves.filter(_._2.flowRate > 0)
    val valveDistances = (START :: valvesWithFlow.keys.toList).combinations(2).flatMap { pairs =>
      (pairs: @unchecked) match {
        case List(a, b) =>
          val dist = shortestPath(a, b, valves)
          Map((a, b) -> dist, (b, a) -> dist)
      }
    }.toMap

    val maxPressures30 = findMaxPressures(valvesWithFlow, valveDistances, 30)
    printDayPart(1, maxPressures30.values.max, "Most pressure released in 30 minutes: %s")

    val maxPressures26 = findMaxPressures(valvesWithFlow, valveDistances, 26)
    val pressuresWithElephant = maxPressures26.map {
      case (myValves, myPressure) =>
        val elephantValves = valvesWithFlow.keySet.diff(myValves)
        elephantValves.subsets().flatMap(maxPressures26.get).map(myPressure + _).max
    }

    printDayPart(2, pressuresWithElephant.max, "Max pressure released with elephant: %s")
  }

  private def shortestPath(from: String, to: String, valves: Map[String, Valve]): Int =
    Dijkstra(from, _ == to, valves(_).tunnels.map((1, _)))._1

  private def findMaxPressures(valves: Map[String, Valve], distances: Map[(String, String), Int], maxMinutes: Int): Map[Set[String], Int] = {
    @tailrec
    def maxPressure(minute: Int, statesPerMinute: Map[Int, Map[State, Int]]): Map[Set[String], Int] =
      if (minute < maxMinutes) {
        val newStatesPerMinute = statesPerMinute.getOrElse(minute, Map.empty).foldLeft(statesPerMinute) {
          case (states, (State(valve, open), pressure)) =>
            valves.filter(v => !open.contains(v._1)).foldLeft(states) {
              case (states, (newValve, newValveData)) =>
                val openMinute = distances((valve, newValve)) + minute + 1
                if (openMinute < maxMinutes) {
                  val state = State(newValve, open + newValve)
                  val pressuresAtMinute = states.getOrElse(openMinute, Map.empty)
                  val oldPressure = pressuresAtMinute.getOrElse(state, 0)
                  val newPressure = pressure + (newValveData.flowRate * (maxMinutes - openMinute))
                  states.updated(openMinute, pressuresAtMinute.updated(state, oldPressure.max(newPressure)))
                } else {
                  states
                }
            }
        }

        maxPressure(minute + 1, newStatesPerMinute)
      } else {
        statesPerMinute.map {
          case (_, states) => states.map(x => x._1.open -> x._2)
        }.flatten.groupMapReduce(_._1)(_._2)(_ max _)
      }

    maxPressure(0, Map(0 -> Map(State(START, Set.empty) -> 0)))
  }

  private case class Valve(flowRate: Int, tunnels: List[String])
  private case class State(valve: String, open: Set[String])
}

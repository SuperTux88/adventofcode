package adventofcode.y2021

import adventofcode.common.NumberHelper.rangeWithReverse
import adventofcode.common.pos.Pos
import adventofcode.common.search.Dijkstra

import scala.io.BufferedSource

object Day23 extends Year2021 {
  override val day = 23

  private val HALLWAY_INDEXES = Set(1, 2, 4, 6, 8, 10, 11)
  private val TARGET_ROOMS = Map(Amber() -> 3, Bronze() -> 5, Copper() -> 7, Desert() -> 9)

  override def runDay(input: BufferedSource): Unit = {
    val initialRooms = parseLines(input.getLines().takeWhile(_.nonEmpty))

    printDayPart(1, getLowestEnergy(initialRooms), "least energy required to organize the amphipods: %s")

    val foldedLines =
      """  #D#C#B#A#
        |  #D#B#A#C#""".stripMargin.linesIterator
    val unfoldedRooms = parseLines(foldedLines)
      .map { case (index, folded) => index -> (initialRooms(index).head :: folded ::: initialRooms(index).tail) }
    printDayPart(2, getLowestEnergy(unfoldedRooms, 4), "least energy required to organize the amphipods after unfold: %s")
  }

  private def getLowestEnergy(startRooms: Map[Int, List[Amphipod]], roomSize: Int = 2): Int = {
    val targetRooms = TARGET_ROOMS.map { case (a, i) => i -> List.fill(roomSize)(a) }
    Dijkstra(
      State(startRooms, Map.empty),
      _.rooms == targetRooms,
      state => getMoveOutStates(state, roomSize) ::: getMoveInStates(state, roomSize)
    )._1
  }

  private def getMoveOutStates(state: State, roomSize: Int): List[(Int, State)] =
    state.rooms.filterNot { case (pos, room) => room.forall(TARGET_ROOMS(_) == pos) }.toList.flatMap {
      (_: @unchecked) match {
        case (roomPos, amphipod :: newRoom) =>
          val targetRoomPos = TARGET_ROOMS(amphipod)
          val targetRoom = state.rooms(targetRoomPos)
          if (targetRoom.forall(_ == amphipod)
            && rangeWithReverse(roomPos, targetRoomPos).forall(p => !state.hallway.contains(p)))
            val dist = roomSize - newRoom.size + (roomPos - targetRoomPos).abs + roomSize - targetRoom.size
            val newRooms = state.rooms.updated(roomPos, newRoom).updated(targetRoomPos, amphipod :: targetRoom)
            List((dist * amphipod.energy, State(newRooms, state.hallway)))
          else
            HALLWAY_INDEXES
              .filter(hallwayPos => rangeWithReverse(roomPos, hallwayPos).forall(p => !state.hallway.contains(p)))
              .map { hallwayPos =>
                ((roomSize - newRoom.size + (roomPos - hallwayPos).abs) * amphipod.energy,
                  State(state.rooms.updated(roomPos, newRoom), state.hallway.updated(hallwayPos, amphipod)))
              }
      }
    }

  private def getMoveInStates(state: State, roomSize: Int): List[(Int, State)] =
    state.hallway.toList.flatMap {
      case (hallwayPos, amphipod) =>
        val roomPos = TARGET_ROOMS(amphipod)
        val room = state.rooms(roomPos)
        if (room.forall(_ == amphipod)
          && rangeWithReverse(roomPos, hallwayPos, false).forall(p => !state.hallway.contains(p)))
          Some((roomSize - room.size + (roomPos - hallwayPos).abs) * amphipod.energy,
            State(state.rooms.updated(roomPos, amphipod :: room), state.hallway.removed(hallwayPos)))
        else
          None
    }

  private def parseLines(lines: Iterator[String]) =
    lines.toList.reverse.foldLeft(TARGET_ROOMS.values.map(_ -> List.empty[Amphipod]).toMap) { (rooms, line) =>
      line.zipWithIndex.foldLeft(rooms) {
        case (rooms, ('A', x)) => rooms.updated(x, Amber() :: rooms(x))
        case (rooms, ('B', x)) => rooms.updated(x, Bronze() :: rooms(x))
        case (rooms, ('C', x)) => rooms.updated(x, Copper() :: rooms(x))
        case (rooms, ('D', x)) => rooms.updated(x, Desert() :: rooms(x))
        case (rooms, _) => rooms
      }
    }

  private sealed trait Amphipod(val energy: Int)
  private case class Amber() extends Amphipod(1)
  private case class Bronze() extends Amphipod(10)
  private case class Copper() extends Amphipod(100)
  private case class Desert() extends Amphipod(1000)

  private case class State(rooms: Map[Int, List[Amphipod]], hallway: Map[Int, Amphipod])
}

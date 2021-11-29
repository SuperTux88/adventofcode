package adventofcode.y2020

import adventofcode.common.pos.{Direction, Pos}

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParMap
import scala.io.BufferedSource

object Day11 extends Year2020 {
  override val day = 11

  override def runDay(input: BufferedSource): Unit = {
    val seats = input.getLines().zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.flatMap {
        case ('.', _) => None
        case ('L', x) => Some(Pos(x, y))
      }
    }.toSet

    val initialEmptySeats = seats.map(_ -> false).toMap.par

    val neighborsMap = seats.map(seat => seat -> seat.neighbors.filter(seats.contains)).toMap

    printDayPart(1, move(initialEmptySeats, neighborsMap), "occupied seats: %s")

    val ferry = Ferry(seats)
    val visibleMap = seats.map { seat =>
      seat -> Direction.directionsWithDiagonals.flatMap(findVisibleSeatInDirection(_, seat, ferry))
    }.toMap

    printDayPart(2, move(initialEmptySeats, visibleMap, 5), "occupied seats with visible rule: %s")
  }

  @tailrec
  private def move(state: ParMap[Pos, Boolean], neighbors: Map[Pos, Seq[Pos]], fullSeatsRule: Int = 4): Int = {
    val next = state.map { cur =>
      val (pos, seat) = cur
      val newSeatState = seat match {
        case false if !neighbors(pos).exists(state(_)) => true
        case true if neighbors(pos).count(state(_)) >= fullSeatsRule => false
        case keep => keep
      }
      pos -> newSeatState
    }

    if (next == state)
      state.values.count(s => s)
    else
      move(next, neighbors, fullSeatsRule)
  }

  @tailrec
  private def findVisibleSeatInDirection(dir: (Int, Int), pos: Pos, ferry: Ferry): Option[Pos] = {
    val nextPos = pos + dir
    if (nextPos.x > ferry.width || nextPos.x < 0 || nextPos.y > ferry.height || nextPos.y < 0)
      None
    else if (ferry.seats.contains(nextPos))
      Some(nextPos)
    else
      findVisibleSeatInDirection(dir, nextPos, ferry)
  }

  private case class Ferry(seats: Set[Pos]) {
    val (width, height) = (seats.maxBy(_.x).x, seats.maxBy(_.y).y)
  }
}

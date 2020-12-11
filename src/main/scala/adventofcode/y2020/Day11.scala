package adventofcode.y2020

import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParMap

object Day11 extends Year2020 {
  override val day = 11

  private val seats = input.getLines().zipWithIndex.flatMap {
    case (line, y) => line.zipWithIndex.flatMap {
      case ('.', _) => None
      case ('L', x) => Some(Pos(x, y))
    }
  }.toSet

  private val (width, height) = (seats.maxBy(_.x).x, seats.maxBy(_.y).y)
  private val initialEmptySeats = seats.map(_ -> false).toMap.par

  private val neighborsMap = seats.map(seat => seat -> seat.neighbors.filter(seats.contains)).toMap

  printDayPart(1, move(initialEmptySeats, neighborsMap), "occupied seats: %s")

  private val visibleMap = seats.map { seat =>
    seat -> Pos.directionsWithDiagonals.flatMap(findVisibleSeatInDirection(_, seat, seats))
  }.toMap

  printDayPart(2, move(initialEmptySeats, visibleMap, 5), "occupied seats with visible rule: %s")

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
  private def findVisibleSeatInDirection(dir: (Int, Int), pos: Pos, seats: Set[Pos]): Option[Pos] = {
    val nextPos = pos + dir
    if (nextPos.x > width || nextPos.x < 0 || nextPos.y > height || nextPos.y < 0)
      None
    else if (seats.contains(nextPos))
      Some(nextPos)
    else
      findVisibleSeatInDirection(dir, nextPos, seats)
  }
}

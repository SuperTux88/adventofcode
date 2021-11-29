package adventofcode.y2016

import adventofcode.common.pos.{Direction, Pos}

import scala.annotation.tailrec

object Day1 extends Year2016 {
  override val day: Int = 1

  private val (startPos, startDirection) = Pos.zero -> 2 // 0, 0 -> moving down

  override def runDay(input: String): Unit = {
    val (positions, _) = input.split(", ").map(_.splitAt(1))
      .foldLeft(Seq(startPos), startDirection)(move)

    printDayPart(1, positions.last.distance(startPos))
    printDayPart(2, findFirstDuplicate(positions).distance(startPos))
  }

  private def move(currentState: (Seq[Pos], Int), instruction: (String, String)) = {
    val (positions, direction) = currentState
    val (turn, blocks) = instruction

    val newDirection = turn match {
      case "L" => Direction.rotateLeft(direction)
      case "R" => Direction.rotateRight(direction)
    }

    val newPositions = (1 to blocks.toInt)
      .scanLeft(positions.last)((pos, _) => pos.moveDirectionIndex(newDirection)).tail
    (positions ++ newPositions, newDirection)
  }

  @tailrec
  private def findFirstDuplicate(positions: Seq[Pos]): Pos = positions match {
    case head :: tail if tail.contains(head) => head
    case _ :: tail => findFirstDuplicate(tail)
  }
}

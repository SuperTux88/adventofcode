package adventofcode.y2016

import scala.annotation.tailrec

object Day1 extends Year2016 {
  override val day: Int = 1

  val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))

  private val (positions, _) = input.mkString.split(", ").map(_.splitAt(1)).foldLeft(Seq(Pos(0, 0)), 0)(move)

  printDayPart(1, positions.last.distance)
  printDayPart(2, findFirstDuplicate(positions).distance)

  private def move(currentState: (Seq[Pos], Int), instruction: (String, String)) = {
    val (positions, direction) = currentState
    val (turn, blocks) = instruction

    val newDirection = turn match {
      case "R" => (direction + 1) % 4
      case "L" => (direction + 3) % 4
    }

    val newPositions = (1 to blocks.toInt).scanLeft(positions.last)((pos, _) => pos.walk(newDirection)).tail
    (positions ++ newPositions, newDirection)
  }

  @tailrec
  private def findFirstDuplicate(positions: Seq[Pos]): Pos = positions match {
    case head :: tail if tail.contains(head) => head
    case _ :: tail => findFirstDuplicate(tail)
  }

  private case class Pos(x: Int, y: Int) {
    def distance: Int = x.abs + y.abs

    def walk(direction: Int): Pos = Pos(x + directions(direction)._1, y + directions(direction)._2)
  }
}

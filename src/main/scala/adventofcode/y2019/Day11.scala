package adventofcode.y2019

import adventofcode.Logging
import adventofcode.common.OCR
import adventofcode.common.pos.{Direction, Pos}

import scala.annotation.tailrec

object Day11 extends Year2019 {
  override val day = 11

  override def runDay(intCode: IntCode): Unit = {
    val painted = move(intCode)
    printDayPart(1, painted.size, "painted panels: %s")

    val message = move(intCode, Map(Pos.zero -> 1).withDefaultValue(0))
    if (Logging.debug) OCR.printImage(message)

    printDayPart(2, OCR.readMessage(message, 8, Pos(5, 6), Pos(1, 0)), "registration identifier: %s")
  }

  @tailrec
  private def move(robot: IntCode, map: Map[Pos, Int] = Map.empty.withDefaultValue(0), pos: Pos = Pos.zero, direction: Int = 0): Map[Pos, Int] = {
    val result = robot.run(map(pos))

    if (result.isRunning) {
      val (paint, turn) = (result.output.next(), result.output.next())

      val newDirection = turn match {
        case 0 => Direction.rotateRight(direction)
        case 1 => Direction.rotateLeft(direction)
      }

      move(result, map.updated(pos, paint.toInt), pos.moveDirectionIndex(newDirection), newDirection)
    } else {
      map
    }
  }
}

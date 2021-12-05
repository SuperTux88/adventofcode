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
    if (Logging.debug) Pos.printMap(message, _ match {
      case 0 => ' '
      case 1 => '█'
    })

    val chars = (0 until 8).map(pos => getCharAt(message, pos))
    printDayPart(2, chars.map(OCR.readChar).mkString, "registration identifier: %s")
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

  private def getCharAt(image: Map[Pos, Int], position: Int) = {
    val start = position * 5 + 1
    (0 until 6).map(y => (start until start + 5).map(x => image(Pos(x, y))))
  }
}

package adventofcode.y2019

import adventofcode.Logging
import adventofcode.common.{OCR, Pos}

import scala.annotation.tailrec

object Day11 extends Year2019 {
  override val day = 11

  private val debug = Logging.debug
  Logging.debug = false

  private val intcode = new IntCode(input.mkString)

  private val painted = move(intcode)
  printDayPart(1, painted.size)

  private val message = move(intcode, Map(Pos.zero -> 1).withDefaultValue(0))
  if (debug) printMap(message)

  private val chars = (0 until 8).map(pos => getCharAt(message, pos))
  printDayPart(2, chars.map(OCR.readChar).mkString, "registration identifier: %s")

  @tailrec
  private def move(robot: IntCode, map: Map[Pos, Int] = Map.empty.withDefaultValue(0), pos: Pos = Pos.zero, direction: Int = 0): Map[Pos, Int] = {
    val result = robot.run(map(pos))

    if (result.isRunning) {
      val (paint, turn) = (result.output.next, result.output.next)

      val newDirection = turn match {
        case 0 => (direction + 3) % 4
        case 1 => (direction + 1) % 4
      }

      move(result, map.updated(pos, paint.toInt), pos.moveDirectionIndex(newDirection), newDirection)
    } else {
      map
    }
  }

  private def printMap(map: Map[Pos, Int]): Unit = {
    (map.keys.map(_.y).min to map.keys.map(_.y).max).foreach( y =>
      println((map.keys.map(_.x).min to map.keys.map(_.x).max).map(x => map(Pos(x, y)) match {
        case 0 => " "
        case 1 => "█"
      }).mkString)
    )
  }

  private def getCharAt(image: Map[Pos, Int], position: Int) = {
    val start = position * 5 + 1
    (0 until 6).map(y => (start until start + 5).map(x => image(Pos(x, y))))
  }
}

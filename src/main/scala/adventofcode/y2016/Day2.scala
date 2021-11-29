package adventofcode.y2016

import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day2 extends Year2016 {
  override val day: Int = 2

  override def runDay(input: BufferedSource): Unit = {
    val instructions = input.getLines().toList

    val keyPad1 = List(List('1', '2', '3'),
                       List('4', '5', '6'),
                       List('7', '8', '9'))
    val keyPad2 = List(List(' ', ' ', '1', ' ', ' '),
                       List(' ', '2', '3', '4', ' '),
                       List('5', '6', '7', '8', '9'),
                       List(' ', 'A', 'B', 'C', ' '),
                       List(' ', ' ', 'D', ' ', ' '))

    printDayPart(1, getCode(instructions, Pos(1, 1), keyPad1), "code: %s")
    printDayPart(2, getCode(instructions, Pos(0, 2), keyPad2), "code: %s")
  }

  private def getCode(instructions: List[String], startPos: Pos, keyPad: List[List[Char]]) =
    instructions.foldLeft("", startPos) { (state, instruction) =>
      val (code, pos) = state
      val newPos = instruction.foldLeft(pos)((pos, direction) => move(pos, direction, keyPad))
      (code + keyAtPos(newPos, keyPad), newPos)
    }._1

  private def move(currentPos: Pos, direction: Char, keyPad: List[List[Char]]) = {
    val newPos = currentPos.move(direction)
    keyAtPos(newPos, keyPad) match {
      case ' ' => currentPos
      case _ => newPos
    }
  }

  private def keyAtPos(pos: Pos, keyPad: List[List[Char]]): Char =
    keyPad.lift(pos.y).getOrElse(List()).lift(pos.x).getOrElse(' ')
}

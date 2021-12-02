package adventofcode.y2021

import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day2 extends Year2021 {
  override val day = 2

  private val InstructionRE = """(\w+) (\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val instructions = input.getLines().takeWhile(_.nonEmpty).map {
      case InstructionRE("forward", distance) => Forward(distance.toInt)
      case InstructionRE("down", value) => Down(value.toInt)
      case InstructionRE("up", value) => Up(value.toInt)
    }.toList

    val finalPos = instructions.foldLeft(Pos(0, 0)) { (pos, instr) =>
      instr match {
        case Forward(dist) => pos + (dist, 0)
        case Down(dist) => pos + (0, dist)
        case Up(dist) => pos + (0, -dist)
      }
    }

    printDayPart(1, finalPos.x * finalPos.y, "product if final position: %s")

    val finalPos2 = instructions.foldLeft((Pos(0, 0), 0)) { (state, instr) =>
      val (pos, aim) = state
      instr match {
        case Forward(dist) => (pos + (dist, aim * dist), aim)
        case Down(x) => (pos, aim + x)
        case Up(x) => (pos, aim - x)
      }
    }._1

    printDayPart(2, finalPos2.x * finalPos2.y, "product if final position with aim: %s")
  }

  private case class Forward(distance: Int)
  private case class Down(value: Int)
  private case class Up(value: Int)
}

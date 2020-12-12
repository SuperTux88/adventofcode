package adventofcode.y2020

import adventofcode.common.pos.Pos

object Day12 extends Year2020 {
  override val day = 12

  private val InstructionRE = """(\w)(\d+)""".r

  private val instructions = input.getLines().map {
    case InstructionRE(action, number) => (action, number.toInt) match {
      case ("F", steps) => Forward(steps)
      case ("R", 90) | ("L", 270) => Rotate(1)
      case ("R" | "L", 180) => Rotate(2)
      case ("R", 270) | ("L", 90) => Rotate(3)
      case ("N", steps) => Move(0, steps)
      case ("E", steps) => Move(1, steps)
      case ("S", steps) => Move(2, steps)
      case ("W", steps) => Move(3, steps)
    }
  }.toSeq

  private val position = instructions.foldLeft(Pos.zero, 1) { (state, instruction) =>
    val (pos, direction) = state
    instruction match {
      case Forward(steps) => (Pos.zero.moveDirectionIndex(direction) * steps + pos, direction)
      case Rotate(stepsClockwise) => (pos, (direction + stepsClockwise) % 4)
      case Move(dir, steps) => (Pos.zero.moveDirectionIndex(dir) * steps + pos, direction)
    }
  }

  printDayPart(1, position._1.distance(Pos.zero), "distance from start: %s")

  private val position2 = instructions.foldLeft(Pos.zero, Pos(10, -1)) { (state, instruction) =>
    val (pos, waypoint) = state
    instruction match {
      case Forward(steps) => (pos + waypoint * steps, waypoint)
      case Rotate(stepsClockwise) => (pos, waypoint.rotate(stepsClockwise))
      case Move(dir, steps) => (pos, Pos.zero.moveDirectionIndex(dir) * steps + waypoint)
    }
  }

  printDayPart(2, position2._1.distance(Pos.zero), "distance from start with waypoints: %s")

  private sealed trait Instruction
  private case class Forward(steps: Int) extends Instruction
  private case class Rotate(stepsClockwise: Int) extends Instruction
  private case class Move(direction: Int, steps: Int) extends Instruction
}

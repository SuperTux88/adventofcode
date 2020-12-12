package adventofcode.y2020

import adventofcode.common.pos.Pos

object Day12 extends Year2020 {
  override val day = 12

  private val InstructionRE = """(\w)(\d+)""".r

  private val instructions = input.getLines().map {
    case InstructionRE(action, number) => (action, number.toInt)
  }.toSeq

  private val position = instructions.foldLeft(Pos.zero, 1) { (state, instruction) =>
    val (pos, direction) = state
    instruction match {
      case ("F", steps) => (Pos.zero.moveDirectionIndex(direction) * steps + pos, direction)
      case ("R", degrees) => (pos, (direction + degrees / 90) % 4)
      case ("L", degrees) => (pos, (direction + degrees / 90 * 3) % 4)
      case ("N", steps) => (Pos.zero.up * steps + pos, direction)
      case ("E", steps) => (Pos.zero.right * steps + pos, direction)
      case ("S", steps) => (Pos.zero.down * steps + pos, direction)
      case ("W", steps) => (Pos.zero.left * steps + pos, direction)
    }
  }

  printDayPart(1, position._1.distance(Pos.zero), "distance from start: %s")

  private val position2 = instructions.foldLeft(Pos.zero, Pos(10, -1)) { (state, instruction) =>
    val (pos, waypoint) = state
    instruction match {
      case ("F", steps) => (pos + waypoint * steps, waypoint)
      case ("R", degrees) => (pos, (1 to degrees / 90).foldLeft(waypoint)((w, _) => Pos(-w.y, w.x)))
      case ("L", degrees) => (pos, (1 to degrees / 90).foldLeft(waypoint)((w, _) => Pos(w.y, -w.x)))
      case ("N", steps) => (pos, Pos.zero.up * steps + waypoint)
      case ("E", steps) => (pos, Pos.zero.right * steps + waypoint)
      case ("S", steps) => (pos, Pos.zero.down * steps + waypoint)
      case ("W", steps) => (pos, Pos.zero.left * steps + waypoint)
    }
  }

  printDayPart(2, position2._1.distance(Pos.zero), "distance from start with waypoints: %s")
}

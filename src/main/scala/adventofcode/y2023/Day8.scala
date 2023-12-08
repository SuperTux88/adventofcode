package adventofcode.y2023

import adventofcode.common.NumberHelper

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day8 extends Year2023 {
  override val day = 8

  private val MapPatternRE = """(\w+) = \((\w+), (\w+)\)""".r

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()
    val instructions = lines.next()

    val map = lines.drop(1).map {
      case MapPatternRE(pos, left, right) => pos -> Instruction(left, right)
    }.toMap

    val numberOfSteps = simulate(instructions, map, "AAA", _ == "ZZZ")
    printDayPart(1, numberOfSteps, "Number of steps until ZZZ: %s")

    val startPositions = map.keys.filter(_.endsWith("A")).toSeq
    val ghostSteps = startPositions.map(simulate(instructions, map, _, _.endsWith("Z")))
    printDayPart(2, NumberHelper.lcm(ghostSteps), "Numbers of steps until all ghosts end on Z: %s")
  }

  private def simulate(instructions: String, map: Map[String, Instruction], start: String, endCondition: String => Boolean): Long = {
    @tailrec
    def step(pos: String, steps: Int = 0): Long = {
      val nextPos = instructions(steps % instructions.length) match {
        case 'L' => map(pos).left
        case 'R' => map(pos).right
      }
      if (endCondition(nextPos)) steps + 1
      else step(nextPos, steps + 1)
    }

    step(start)
  }

  private case class Instruction(left: String, right: String)
}

package adventofcode.y2023

import adventofcode.common.pos.{Direction, Pos}

import scala.io.BufferedSource

object Day18 extends Year2023 {
  override val day = 18

  private val InstructionRE = """([UDLR]) (\d+) \(#(\w{6})\)""".r

  override def runDay(input: BufferedSource): Unit = {
    val instructions = input.getLines().takeWhile(_.nonEmpty).map {
      case InstructionRE(direction, dist, color) =>
        val dir = direction match {
          case "U" => Direction.upIndex
          case "D" => Direction.downIndex
          case "L" => Direction.leftIndex
          case "R" => Direction.rightIndex
        }
        Instruction(dir, dist.toInt, color)
    }.toList

    val border = getBorderPoints(instructions, _.dir, _.dist)
    printDayPart(1, calculateArea(border), "Size of area: %s")

    val realBorder = getBorderPoints(instructions, _.realDir, _.realDist)
    printDayPart(2, calculateArea(realBorder), "Size of area with correct instructions: %s")
  }

  private def getBorderPoints(instructions: List[Instruction], dir: Instruction => Int, dist: Instruction => Int) =
    instructions.foldLeft(List(Pos.zero)) { (border, inst) =>
      border.head.moveDirectionIndex(dir(inst), dist(inst)) :: border
    }

  private def calculateArea(border: List[Pos]) = {
    val pairs = (border.last, border.head) :: border.sliding(2).map { pairs =>
      (pairs: @unchecked) match {
        case List(a, b) => (a, b)
      }
    }.toList

    val borderDistance = pairs.map((a, b) => a.distance(b)).sum
    // https://en.wikipedia.org/wiki/Shoelace_formula
    val area = pairs.map((a, b) => (a.y + b.y).toLong * (a.x - b.x).toLong).sum.abs / 2

    (borderDistance / 2) + area + 1
  }

  private case class Instruction(dir: Int, dist: Int, color: String) {
    val realDir: Int = color.last match {
      case '0' => Direction.rightIndex
      case '1' => Direction.downIndex
      case '2' => Direction.leftIndex
      case '3' => Direction.upIndex
    }

    val realDist: Int = Integer.parseInt(color.init, 16)
  }
}

package adventofcode.y2022

import scala.io.BufferedSource

object Day5 extends Year2022 {
  override val day = 5

  private val InstructionRE = """move (\d+) from (\d+) to (\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()

    val stacks = lines.takeWhile(!_.isBlank)
      .map(_.drop(1).zipWithIndex.filter(_._2 % 4 == 0).map(_._1).toList)
      .toList.dropRight(1).transpose.map(_.filter(_ != ' '))

    val procedure = lines.map {
      case InstructionRE(n, from, to) => Instruction(n.toInt, from.toInt - 1, to.toInt - 1)
    }.toList

    val finalStacks = runProcedure(procedure, stacks)
    printDayPart(1, finalStacks.map(_.head).mkString, "Top crates after procedure: %s")

    val finalStacks2 = runProcedure(procedure, stacks, 9001)
    printDayPart(2, finalStacks2.map(_.head).mkString, "Top crates after procedure with CrateMover 9001: %s")
  }

  private def runProcedure(procedure: List[Instruction], stacks: List[List[Char]], craneModel: Int = 9000) =
    procedure.foldLeft(stacks) { (currentStack, instruction) =>
      instruction match {
        case Instruction(number, from, to) =>
          val cratesToMove = currentStack(from).take(number)
          val newCrates = if (craneModel == 9000) cratesToMove.reverse else cratesToMove
          currentStack.updated(from, currentStack(from).drop(number)).updated(to, newCrates ::: currentStack(to))
      }
    }

  private case class Instruction(number: Int, from: Int, to: Int)
}

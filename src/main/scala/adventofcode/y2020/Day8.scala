package adventofcode.y2020

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day8 extends Year2020 {
  override val day = 8

  private val InstructionRE = """(\w+) ([+-]\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val instructions = input.getLines().map {
      case InstructionRE("acc", param) => Acc(param.toInt)
      case InstructionRE("jmp", param) => Jmp(param.toInt)
      case InstructionRE("nop", param) => Nop(param.toInt)
    }.toVector

    val result = run(instructions)
    printDayPart(1, result.accumulator, "accumulator value before loop: %s")

    val changedInstructions = result.executedInstructions.iterator.flatMap { instruction =>
      instructions(instruction) match {
        case Acc(_) => None
        case Jmp(param) => Some(instructions.updated(instruction, Nop(param)))
        case Nop(param) => Some(instructions.updated(instruction, Jmp(param)))
      }
    }

    val fixedResult = changedInstructions.map(run(_)).find(!_.looped).get
    printDayPart(2, fixedResult.accumulator, "accumulator value after fixing: %s")
  }

  @tailrec
  private def run(instructions: Vector[Instruction], accumulator: Int = 0, pointer: Int = 0, executed: Set[Int] = Set()): Result = {
    if (executed.contains(pointer))
      Result(accumulator, executed, looped = true)
    else if (pointer == instructions.size)
      Result(accumulator, executed, looped = false)
    else
      instructions(pointer) match {
        case Acc(param) => run(instructions, accumulator + param, pointer + 1, executed + pointer)
        case Jmp(param) => run(instructions, accumulator, pointer + param, executed + pointer)
        case Nop(_) => run(instructions, accumulator, pointer + 1, executed + pointer)
      }
  }

  private sealed trait Instruction
  private case class Acc(param: Int) extends Instruction
  private case class Jmp(param: Int) extends Instruction
  private case class Nop(param: Int) extends Instruction

  private case class Result(accumulator: Int, executedInstructions: Set[Int], looped: Boolean)
}

package adventofcode.y2024

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day17 extends Year2024 {
  override val day = 17

  private val RegisterRE = """Register [ABC]: (\d+)""".r
  private val ProgramRE = """Program: ([\d,]+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()
    val registers = lines.take(3).map {
      case RegisterRE(value) => value.toLong
    }.toList
    val program = lines.drop(1).next().match {
      case ProgramRE(program) => program.split(",").map(_.toInt).toList
    }

    val output = runProgram(registers, program)
    printDayPart(1, output.mkString(","), "P1: %s")
  }

  @tailrec
  private def runProgram(registers: List[Long], program: List[Int], pointer: Int = 0, output: List[Int] = List.empty): List[Int] = {
    // println(s"registers: $registers, program: $program, pointer: $pointer, output: $output")
    if (pointer >= program.length) output.reverse
    else {
      val List(a, b, c) = registers
      val opcode = program(pointer)
      val operand = program(pointer + 1)
      opcode match {
        case 0 => // adv
          runProgram(List(dv(a, operand, registers), b, c), program, pointer + 2, output)
        case 1 => // bxl
          runProgram(List(a, b ^ operand, c), program, pointer + 2, output)
        case 2 => // bst
          runProgram(List(a, getCombo(operand, registers) % 8, c), program, pointer + 2, output)
        case 3 => // jnz
          if (a != 0) runProgram(registers, program, operand, output)
          else runProgram(registers, program, pointer + 2, output)
        case 4 => // bxc
          runProgram(List(a, b ^ c, c), program, pointer + 2, output)
        case 5 => // out
          val value = getCombo(operand, registers) % 8
          runProgram(registers, program, pointer + 2, value.toInt :: output)
        case 6 => // bdv
          runProgram(List(a, dv(a, operand, registers), c), program, pointer + 2, output)
        case 7 => // cdv
          runProgram(List(a, b, dv(a, operand, registers)), program, pointer + 2, output)
        case _ => throw new IllegalArgumentException(s"Invalid opcode: $opcode")
      }
    }
  }

  private def getCombo(value: Int, registers: List[Long]): Long =
    value match {
      case 0 | 1 | 2 | 3 => value
      case 4 => registers.head
      case 5 => registers(1)
      case 6 => registers(2)
      case _ => throw new IllegalArgumentException(s"Invalid combo value: $value")
    }

  private def dv(a: Long, operand: Int, registers: List[Long]): Long =
    (a / math.pow(2, getCombo(operand, registers).toDouble)).toLong
}

package adventofcode.y2018

import scala.annotation.tailrec

class ElfCode(private val lines: Iterator[String]) {
  private val InstructionRE = """(\w+) (\d+) (\d+) (\d+)""".r

  val ip: Int = lines.take(1).mkString.split(" ")(1).toInt

  val program: Vector[(String, Vector[Int])] = lines.map {
    case InstructionRE(opcode, a, b, c) => (opcode, Vector(a.toInt, b.toInt, c.toInt))
  }.toVector

  def size: Int = program.size

  @tailrec
  final def runProgram(registers: Vector[Int] = Vector.fill(6)(0)): Vector[Int] = {
    if (registers(ip) < size)
      runProgram(runInstruction(registers))
    else
      registers
  }

  def runInstruction(registers: Vector[Int]): Vector[Int] = {
    val instruction = program(registers(ip))
    val newRegisters = ElfCode.execute(instruction._1, instruction._2, registers)
    newRegisters.updated(ip, newRegisters(ip) + 1)
  }
}

object ElfCode {
  val opcodes: Set[String] = Set(
    "addr", "addi",
    "mulr", "muli",
    "banr", "bani",
    "borr", "bori",
    "setr", "seti",
    "gtir", "gtri", "gtrr",
    "eqir", "eqri", "eqrr"
  )

  def execute(opcode: String, params: Vector[Int], reg: Vector[Int]): Vector[Int] = {
    implicit def bool2int(bool: Boolean): Int = if (bool) 1 else 0

    val Seq(a, b, c) = params
    val result: Int = opcode match {
      case "addr" => reg(a) + reg(b)
      case "addi" => reg(a) + b
      case "mulr" => reg(a) * reg(b)
      case "muli" => reg(a) * b
      case "banr" => reg(a) & reg(b)
      case "bani" => reg(a) & b
      case "borr" => reg(a) | reg(b)
      case "bori" => reg(a) | b
      case "setr" => reg(a)
      case "seti" => a
      case "gtir" => a > reg(b)
      case "gtri" => reg(a) > b
      case "gtrr" => reg(a) > reg(b)
      case "eqir" => a == reg(b)
      case "eqri" => reg(a) == b
      case "eqrr" => reg(a) == reg(b)
    }
    reg.updated(c, result)
  }
}

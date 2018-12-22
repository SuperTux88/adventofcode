package adventofcode.y2018

import scala.annotation.tailrec

class ElfCode(private val lines: Iterator[String]) {
  private val InstructionRE = """(\w+) (\d+) (\d+) (\d+)""".r

  val ip: Int = lines.take(1).mkString.split(" ")(1).toInt

  val program: Vector[(String, Vector[Int])] = lines.map {
    case InstructionRE(opcode, a, b, c) => (opcode, Vector(a.toInt, b.toInt, c.toInt))
  }.toVector

  private val parsedInstructions = program.map {
    case (opcode, params) => ElfCode.getInstruction(opcode, params)
  }

  def size: Int = program.size

  @tailrec
  final def runProgram(registers: Vector[Int] = Vector.fill(6)(0)): Vector[Int] = {
    if (registers(ip) < size)
      runProgram(runInstruction(registers))
    else
      registers
  }

  def runInstruction(registers: Vector[Int]): Vector[Int] = {
    val newRegisters = parsedInstructions(registers(ip))(registers)
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

  def getInstruction(opcode: String, params: Vector[Int]): Vector[Int] => Vector[Int] = {
    val instruction = parseInstriction(opcode)
    val Seq(a, b, c) = params
    reg => reg.updated(c, instruction(a, b, reg))
  }

  private def parseInstriction(opcode: String): (Int, Int, Vector[Int]) => Int = {
    implicit def bool2int(bool: Boolean): Int = if (bool) 1 else 0

    opcode match {
      case "addr" => (a, b, reg) => reg(a) + reg(b)
      case "addi" => (a, b, reg) => reg(a) + b
      case "mulr" => (a, b, reg) => reg(a) * reg(b)
      case "muli" => (a, b, reg) => reg(a) * b
      case "banr" => (a, b, reg) => reg(a) & reg(b)
      case "bani" => (a, b, reg) => reg(a) & b
      case "borr" => (a, b, reg) => reg(a) | reg(b)
      case "bori" => (a, b, reg) => reg(a) | b
      case "setr" => (a, _, reg) => reg(a)
      case "seti" => (a, _, _)   => a
      case "gtir" => (a, b, reg) => a > reg(b)
      case "gtri" => (a, b, reg) => reg(a) > b
      case "gtrr" => (a, b, reg) => reg(a) > reg(b)
      case "eqir" => (a, b, reg) => a == reg(b)
      case "eqri" => (a, b, reg) => reg(a) == b
      case "eqrr" => (a, b, reg) => reg(a) == reg(b)
    }
  }
}

package adventofcode.y2016

object Day12 extends Year2016 {
  override val day: Int = 12

  val CpyRE = """cpy (\w+) (\w)""".r
  val IncRE = """inc (\w)""".r
  val DecRE = """dec (\w)""".r
  val JnzRE = """jnz (\w) ([\w-]+)""".r

  val ValRE = """(\d+)""".r
  val RegRE = """(\w)""".r

  private val instructions = input.getLines.map {
    case CpyRE(from, to)    => Copy(valueOrRegisterKey(from), to)
    case IncRE(reg)         => Inc(reg)
    case DecRE(reg)         => Dec(reg)
    case JnzRE(cond, steps) => Jump(valueOrRegisterKey(cond), steps.toInt)
  }.toList

  val registers = Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0)

  printDayPart(1, runInstructions(registers)("a"))
  printDayPart(2, runInstructions(registers + ("c" -> 1))("a"))

  private def runInstructions(startRegisters: Map[String, Int]) = {
    var registers = startRegisters
    var currentPos = 0

    while (currentPos < instructions.length) {
      instructions(currentPos) match {
        case Copy(from, to) => registers += (to -> valueOrRegisterValue(from, registers))
        case Inc(reg) => registers += (reg -> (registers(reg) + 1))
        case Dec(reg) => registers += (reg -> (registers(reg) - 1))
        case Jump(cond, skip) =>
          if (valueOrRegisterValue(cond, registers) != 0)
            currentPos += skip.toInt - 1
      }
      currentPos += 1
    }

    registers
  }

  private def valueOrRegisterKey(string: String) = string match {
    case ValRE(value) => value.toInt
    case RegRE(reg) => reg
  }

  private def valueOrRegisterValue(string: Any, registers: Map[String, Int]) = string match {
    case value: Int => value
    case reg: String => registers(reg)
  }

  private trait Instruction
  private case class Copy(from: Any, to: String) extends Instruction
  private case class Inc(reg: String) extends Instruction
  private case class Dec(reg: String) extends Instruction
  private case class Jump(cond: Any, steps: Int) extends Instruction
}
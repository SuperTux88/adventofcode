package adventofcode.y2016

import adventofcode.Logging

class Computer(instructionLines: Iterator[String]) {

  private val CpyRE = """cpy ([\w-]+) (\w)""".r
  private val IncRE = """inc (\w)""".r
  private val DecRE = """dec (\w)""".r
  private val JnzRE = """jnz (\w+) ([\w-]+)""".r
  private val TglRE = """tgl (\w+)""".r
  private val OutRE = """out (\w+)""".r

  private val ValRE = """(-?\d+)""".r
  private val RegRE = """(\w)""".r

  private val instructions: List[Instruction] = instructionLines.map {
    case CpyRE(from, to)    => Copy(valueOrRegisterKey(from), to)
    case IncRE(reg)         => Inc(reg)
    case DecRE(reg)         => Dec(reg)
    case JnzRE(cond, steps) => Jump(valueOrRegisterKey(cond), valueOrRegisterKey(steps))
    case TglRE(target)      => Toggle(valueOrRegisterKey(target))
    case OutRE(reg)         => Out(reg)
  }.toList

  private val registers = Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0)

  def run: Map[String, Int] = runInstructions(None, i => println(i))
  def run(initValue: (String, Int), out: Int => Unit = i => println(i)): Map[String, Int] =
    runInstructions(Some(initValue), out)

  private def runInstructions(initValue: Option[(String, Int)], out: Int => Unit) = {
    var registers = if (initValue.isDefined) this.registers + initValue.get else this.registers
    var instructions = this.instructions
    var currentPos = 0

    while (currentPos < instructions.length) {
      instructions(currentPos) match {
        case Copy(from, to) if registers.contains(to) =>
          registers += (to -> valueOrRegisterValue(from, registers))
        case Inc(reg) if registers.contains(reg) =>
          registers += (reg -> (registers(reg) + 1))
        case Dec(reg) if registers.contains(reg) =>
          registers += (reg -> (registers(reg) - 1))
        case Jump(cond, skip) =>
          if (valueOrRegisterValue(cond, registers) != 0)
            currentPos += valueOrRegisterValue(skip, registers) - 1
        case Toggle(target) =>
          val targetInst = valueOrRegisterValue(target, registers) + currentPos
          if (instructions.length > targetInst)
            instructions = instructions.updated(targetInst, instructions(targetInst).toggle)
        case Out(reg) => out(registers(reg))
        case inst => if (Logging.debug) println(s"skipped $inst")
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

  private trait Instruction {
    def toggle: Instruction
  }
  private case class Copy(from: Any, to: String) extends Instruction {
    override def toggle: Instruction = Jump(from, valueOrRegisterKey(to))
  }
  private case class Inc(reg: String) extends Instruction {
    override def toggle: Instruction = Dec(reg)
  }
  private case class Dec(reg: String) extends Instruction {
    override def toggle: Instruction = Inc(reg)
  }
  private case class Jump(cond: Any, steps: Any) extends Instruction {
    override def toggle: Instruction = Copy(cond, steps.toString)
  }
  private case class Toggle(target: Any) extends Instruction {
    override def toggle: Instruction = Inc(target.toString)
  }
  private case class Out(reg: String) extends Instruction {
    override def toggle: Instruction = Inc(reg)
  }
}

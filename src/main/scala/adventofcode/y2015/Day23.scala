package adventofcode.y2015

object Day23 extends Year2015 {
  override val day: Int = 23

  val CommandRE = """(\w+) ([ab])""".r
  val JumpCommandRE = """jmp ([+-]\d+)""".r
  val JumpIfCommandRE = """(ji[oe]) ([ab]), ([+-]\d+)""".r

  private val commands = input.getLines.map {
    case CommandRE(command, register) => Command(command, Some(register(0)), None)
    case JumpCommandRE(offset) => Command("jmp", None, Some(offset.toInt))
    case JumpIfCommandRE(command, register, offset) => Command(command, Some(register(0)), Some(offset.toInt))
  }.toList

  printDayPart(1, runCommands(Map())('b').toInt)
  printDayPart(2, runCommands(Map('a' -> 1l))('b').toInt)

  private def runCommands(initialRegisters: Map[Char, Long]) = {
    var registers = initialRegisters.withDefaultValue(0l)
    var currentCommand = 0

    while(currentCommand >= 0 && currentCommand < commands.size) {
      commands(currentCommand) match {
        case Command("hlf", Some(reg), _) =>
          registers += reg -> registers(reg) / 2
          currentCommand += 1
        case Command("tpl", Some(reg), _) =>
          registers += reg -> registers(reg) * 3
          currentCommand += 1
        case Command("inc", Some(reg), _) =>
          registers += reg -> (registers(reg) + 1)
          currentCommand += 1
        case Command("jmp", _, Some(offset)) => currentCommand += offset
        case Command("jie", Some(reg), Some(offset)) => currentCommand += (if(registers(reg) % 2 == 0) offset else 1)
        case Command("jio", Some(reg), Some(offset)) => currentCommand += (if(registers(reg) == 1) offset else 1)
        case command => throw new MatchError(s"Invalid $command")
      }
    }

    registers
  }

  private case class Command(command: String, register: Option[Char], offset: Option[Int])
}

package adventofcode.y2018

object Day16 extends Year2018 {
  override val day = 16

  val SampleRE ="""Before: \[(\d, \d, \d, \d)\]
                  |(\d+ \d \d \d)
                  |After:  \[(\d, \d, \d, \d)\]""".stripMargin.r

  val InstructionRE = """(\d+) (\d) (\d) (\d)""".r

  val inputStr = input.mkString
  val (samples, program) = inputStr.splitAt(inputStr.indexOf("\n\n\n\n"))

  val possibleOpcodes = samples.split("\n\n").map {
    case SampleRE(before, instruction, after) =>
      val (regBefore, regAfter) = (parseRegister(before), parseRegister(after))
      val Array(opcode, a, b, c) = instruction.split(" ").map(_.toInt)
      (opcode, (0 to 15).filter { possibleOpcode =>
        execute(possibleOpcode, a, b, c, regBefore) == regAfter
      })
  }

  printDayPart(1, possibleOpcodes.count(_._2.size >= 3))

  val possibleOpcodeMappings = possibleOpcodes
    .foldLeft(Map[Int, Set[Int]]().withDefaultValue((0 to 15).toSet)) { (possibleOpcodeMappings, result) =>
      val (resOpcode, matchingOpcodes) = result
      possibleOpcodeMappings + (resOpcode -> possibleOpcodeMappings(resOpcode).filter(matchingOpcodes.contains(_)))
  }

  val opcodeMapping = (0 to 15).foldLeft(Map[Int, Int](), possibleOpcodeMappings) { (mappings, _) =>
    val (finalMapping, possibleMappings) = mappings
    val (opcode, mapped) = possibleMappings.find(_._2.size == 1).get
    (finalMapping + (opcode -> mapped.head), possibleMappings.mapValues(v => v - mapped.head))
  }._1

  val registers = program.trim.split("\n").foldLeft(List.fill(4)(0)) {
    case (state, InstructionRE(instruction, a, b, c)) =>
      execute(opcodeMapping(instruction.toInt), a.toInt, b.toInt, c.toInt, state)
  }

  printDayPart(2, registers.head)

  def parseRegister(str: String) = str.split(", ").map(_.toInt).toList

  def execute(opcode: Int, a: Int, b: Int, c: Int, reg: List[Int]): List[Int] = {
    implicit def bool2int(bool: Boolean): Int = if (bool) 1 else 0

    val result: Int = opcode match {
      case  0 => reg(a) + reg(b)  // addr
      case  1 => reg(a) + b       // addi
      case  2 => reg(a) * reg(b)  // mulr
      case  3 => reg(a) * b       // muli
      case  4 => reg(a) & reg(b)  // banr
      case  5 => reg(a) & b       // bani
      case  6 => reg(a) | reg(b)  // borr
      case  7 => reg(a) | b       // bori
      case  8 => reg(a)           // setr
      case  9 => a                // seti
      case 10 => a > reg(b)       // gtir
      case 11 => reg(a) > b       // gtri
      case 12 => reg(a) > reg(b)  // gtrr
      case 13 => a == reg(b)      // eqir
      case 14 => reg(a) == b      // eqri
      case 15 => reg(a) == reg(b) // eqrr
    }
    reg.updated(c, result)
  }
}

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
      (opcode, ElfCode.opcodes.filter { possibleOpcode =>
        ElfCode.getInstruction(possibleOpcode, Vector(a, b, c))(regBefore) == regAfter
      })
  }

  printDayPart(1, possibleOpcodes.count(_._2.size >= 3))

  val possibleOpcodeMappings = possibleOpcodes
    .foldLeft(Map[Int, Set[String]]().withDefaultValue(ElfCode.opcodes)) { (possibleOpcodeMappings, result) =>
      val (resOpcode, matchingOpcodes) = result
      possibleOpcodeMappings + (resOpcode -> possibleOpcodeMappings(resOpcode).intersect(matchingOpcodes))
  }

  val opcodeMapping = ElfCode.opcodes.foldLeft(Map[Int, String](), possibleOpcodeMappings) { (mappings, _) =>
    val (finalMapping, possibleMappings) = mappings
    val (opcode, mapped) = possibleMappings.find(_._2.size == 1).get
    (finalMapping + (opcode -> mapped.head), possibleMappings.view.mapValues(v => v - mapped.head).toMap)
  }._1

  val registers = program.trim.split("\n").foldLeft(Vector.fill(4)(0)) {
    case (state, InstructionRE(instruction, a, b, c)) =>
      ElfCode.getInstruction(opcodeMapping(instruction.toInt), Vector(a.toInt, b.toInt, c.toInt))(state)
  }

  printDayPart(2, registers.head)

  def parseRegister(str: String) = str.split(", ").map(_.toInt).toVector
}

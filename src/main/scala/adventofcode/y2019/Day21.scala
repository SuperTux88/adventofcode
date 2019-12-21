package adventofcode.y2019

object Day21 extends Year2019 {
  override val day = 21

  private val intCode = new IntCode(inputString)

  private val program1 =
    """NOT A J  # if NOT A
      |NOT C T
      |OR T J   # OR NOT C
      |AND D J  # but D, then JUMP!
      |WALK
      |""".stripMargin
  printDayPart(1, runSpringScript(program1))

  private val program2 =
    """NOT A J  # if NOT A
      |NOT B T
      |OR T J   # OR NOT B
      |NOT C T
      |OR T J   # OR NOT C
      |AND D J  # but D, then JUMP!
      |NOT E T
      |NOT T T  # but only if E
      |OR H T   # OR H
      |AND T J  # then really JUMP!!
      |RUN
      |""".stripMargin
  printDayPart(2, runSpringScript(program2))

  private def runSpringScript(program: String): Long =
    program.split('\n').foldLeft((intCode.startAsciiProgram()._1, List.empty[Long])) {
      case ((droid, _), nextLine) => droid.sendAsciiInput(stripComment(nextLine))
    }._2.last

  private def stripComment(line: String) =
    if (line.contains('#'))
      line.slice(0, line.indexOf('#')).trim
    else
      line
}

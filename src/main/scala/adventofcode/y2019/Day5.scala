package adventofcode.y2019

import adventofcode.Logging

object Day5 extends Year2019 {
  override val day = 5

  IntCode.debug = Logging.debug

  private val intCode = new IntCode(inputString)

  printDayPart(1, intCode.run(1).output.dropWhile(_ == 0).next(), "diagnostic code: %s")
  printDayPart(2, intCode.run(5).output.next(), "diagnostic code: %s")
}

package adventofcode.y2019

object Day9 extends Year2019 {
  override val day = 9

  private val intcode = new IntCode(input.mkString)

  printDayPart(1, intcode.run(1).output.last)
  printDayPart(2, intcode.run(2).output.last)
}

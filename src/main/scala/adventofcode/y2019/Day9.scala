package adventofcode.y2019

object Day9 extends Year2019 {
  override val day = 9

  private val intCode = new IntCode(input.mkString)

  printDayPart(1, intCode.run(1).output.next)
  printDayPart(2, intCode.run(2).output.next)
}

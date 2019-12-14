package adventofcode.y2019

object Day5 extends Year2019 {
  override val day = 5

  private val intcode = new IntCode(input.mkString)

  printDayPart(1, intcode.run(1).output.dropWhile(_ == 0).next)
  printDayPart(2, intcode.run(5).output.next)
}

package adventofcode.y2019

object Day5 extends Year2019 {
  override val day = 5

  val intcode = new IntCode(input.mkString)

  printDayPart(1, intcode.start(Vector(1)).output.last)
  printDayPart(2, intcode.start(Vector(5)).output.last)
}

package adventofcode.y2019

object Day9 extends Year2019 {
  override val day = 9

  private val intCode = new IntCode(inputString)

  printDayPart(1, intCode.run(1).output.next, "BOOST keycode: %s")
  printDayPart(2, intCode.run(2).output.next, "coordinates of the distress signal: %s")
}

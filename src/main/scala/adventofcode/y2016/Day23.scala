package adventofcode.y2016

object Day23 extends Year2016 {
  override val day = 23

  val computer = new Computer(input.getLines)

  printDayPart(1, computer.run("a" -> 7)("a"))
  printDayPart(2, computer.run("a" -> 12)("a"))
}

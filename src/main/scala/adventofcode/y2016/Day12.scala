package adventofcode.y2016

object Day12 extends Year2016 {
  override val day: Int = 12

  val computer = new Computer(input.getLines())

  printDayPart(1, computer.run("a"))
  printDayPart(2, computer.run("c" -> 1)("a"))
}

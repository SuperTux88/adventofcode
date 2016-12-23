package adventofcode.y2016

object Day12 extends Year2016 {
  override val day: Int = 12

  val computer = new Computer(input.getLines)
  val registers = Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0)

  printDayPart(1, computer.run(registers)("a"))
  printDayPart(2, computer.run(registers + ("c" -> 1))("a"))
}

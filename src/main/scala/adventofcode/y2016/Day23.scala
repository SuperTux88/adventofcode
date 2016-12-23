package adventofcode.y2016

object Day23 extends Year2016 {
  override val day = 23

  val computer = new Computer(input.getLines)
  val registers = Map("a" -> 7, "b" -> 0, "c" -> 0, "d" -> 0)

  printDayPart(1, computer.run(registers)("a"))
  printDayPart(2, computer.run(registers + ("a" -> 12))("a"))
}

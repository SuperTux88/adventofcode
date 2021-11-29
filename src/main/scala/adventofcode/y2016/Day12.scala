package adventofcode.y2016

import scala.io.BufferedSource

object Day12 extends Year2016 {
  override val day: Int = 12

  override def runDay(input: BufferedSource): Unit = {
    val computer = new Computer(input.getLines())

    printDayPart(1, computer.run("a"))
    printDayPart(2, computer.run("c" -> 1)("a"))
  }
}

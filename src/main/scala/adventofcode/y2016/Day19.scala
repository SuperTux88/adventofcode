package adventofcode.y2016

object Day19 extends Year2016 {
  override val day = 19

  override def runDay(input: String): Unit = {
    val elves = input.toInt

    printDayPart(1, josephus(elves))
    printDayPart(2, part2(elves))
  }

  private def josephus(elves: Int) =
    Integer.parseInt(elves.toBinaryString.substring(1) + 1, 2)

  private def part2(elves: Int) = {
    var counter = 1
    while (counter * 3 < elves) counter *= 3

    elves - counter + Math.max(elves - 2 * counter, 0)
  }
}

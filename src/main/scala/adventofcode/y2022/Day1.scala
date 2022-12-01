package adventofcode.y2022

object Day1 extends Year2022 {
  override val day = 1

  override def runDay(input: String): Unit = {
    val elves = input.split("\n\n").map(_.split('\n').map(_.toInt))
    val elvesCalories = elves.map(_.sum).sorted

    printDayPart(1, elvesCalories.last, "The elf carrying the most has %s calories")
    printDayPart(2, elvesCalories.takeRight(3).sum, "The top 3 elves carry %s calories in total")
  }
}

package adventofcode

object Day1 extends DayApp {
  override val day: Int = 1

  var floor = 0
  var basementIndex = 0

  input.zipWithIndex.foreach { case (char, index) =>
    char match {
      case '(' => floor += 1
      case ')' => floor -= 1
    }

    if (floor < 0 && basementIndex == 0) basementIndex = index+1
  }

  printDayPart(1, floor, "end floor: %s")
  printDayPart(2, basementIndex, "first in basement after: %s")
}

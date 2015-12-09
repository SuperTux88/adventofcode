package adventofcode

object Day1 extends DayApp {
  override val day: Int = 1

  var floor = 0
  var basementIndex = 0

  Input(1).withIndex.foreach { case (char, index) =>
    char match {
      case '(' => floor += 1
      case ')' => floor -= 1
    }

    if (floor < 0 && basementIndex == 0) basementIndex = index+1
  }

  printDayPart(1, s"end floor: $floor")
  printDayPart(2, s"first in basement after: $basementIndex")
}

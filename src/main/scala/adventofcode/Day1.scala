package adventofcode

object Day1 extends App {
  var floor = 0
  var basementIndex = 0

  Input(1).withIndex.foreach { case (char, index) =>
    char match {
      case '(' => floor += 1
      case ')' => floor -= 1
    }

    if (floor < 0 && basementIndex == 0) basementIndex = index+1
  }

  println(s"end floor: $floor")
  println(s"first in basement after: $basementIndex")
}

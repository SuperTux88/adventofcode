package adventofcode

object Day3 extends DayApp {
  override val day: Int = 3

  printDayPart(1, s"total $part1 visits in unique houses")
  printDayPart(2, s"total $part2 visits in unique houses")

  private def part1: Int = {
    val pos = Pos(0, 0)
    (Pos(0, 0) :: Input(3).source.map(move(_, pos)).toList).distinct.size
  }

  private def part2: Int = {
    val santaPos, roboSantaPos = Pos(0, 0)

    val houses = Pos(0, 0) :: Input(3).withIndex.map { case (char, index) =>
      val pos = if (index % 2 == 0) santaPos else roboSantaPos
      move(char, pos)
    }.toList

    houses.distinct.size
  }

  private def move(c: Char, pos: Pos): Pos = {
    c match {
      case 'v' => pos.y -= 1
      case '^' => pos.y += 1
      case '<' => pos.x -= 1
      case '>' => pos.x += 1
    }
    pos.copy()
  }

  private case class Pos(var x: Int, var y: Int)
}

package adventofcode

object Day3 extends App {
  part1()
  part2()

  private def part1() {
    val pos = Pos(0, 0)
    val houses = Pos(0, 0) :: Input(3).source.map(move(_, pos)).toList

    println(s"PART 1: total ${houses.distinct.size} visits in unique houses")
  }

  private def part2() {
    val santaPos = Pos(0, 0)
    val roboSantaPos = Pos(0, 0)

    val houses = Pos(0, 0) :: Input(3).withIndex.map { case (char, index) =>
      val pos = if (index % 2 == 0) santaPos else roboSantaPos
      move(char, pos)
    }.toList

    println(s"PART 2: total ${houses.distinct.size} visits in unique houses")
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

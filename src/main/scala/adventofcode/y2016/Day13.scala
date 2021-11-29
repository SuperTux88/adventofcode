package adventofcode.y2016

import adventofcode.common.pos.Pos

object Day13 extends Year2016 {
  override val day: Int = 13

  override def runDay(input: String): Unit = {
    val favoriteNumber = input.toInt
    val start = Pos(1, 1)
    val target = Pos(31, 39)

    var steps = 0
    var mazeSteps = Map(start -> steps)
    var currentPositions = List(start)

    val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))

    while (!mazeSteps.contains(target)) {
      currentPositions = currentPositions.flatMap { pos =>
        directions.map(pos + _).filter(pos => pos.positive && !isWall(pos, favoriteNumber) && !mazeSteps.contains(pos))
      }.distinct
      steps += 1
      mazeSteps ++= currentPositions.map(_ -> steps)
    }

    printDayPart(1, mazeSteps(target))
    printDayPart(2, mazeSteps.count(_._2 <= 50))
  }

  private def isWall(pos: Pos, favoriteNumber: Int): Boolean = {
    def countBinary(i: Int): Int = i match {
      case 0|1 => i
      case _ => countBinary(i/2) + i%2
    }

    val Pos(x, y) = pos
    countBinary(x*x + 3*x + 2*x*y + y + y*y + favoriteNumber) % 2 == 1
  }
}

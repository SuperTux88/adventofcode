package adventofcode.y2016

import adventofcode.common.Pos

object Day13 extends Year2016 {
  override val day: Int = 13

  private val favoriteNumber = input.mkString.toInt
  private val start = Pos(1,1)
  private val target = Pos(31,39)

  private var steps = 0
  private var mazeSteps = Map(start -> steps)
  private var currentPositions = List(start)

  private val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))

  while(!mazeSteps.contains(target)) {
    currentPositions = currentPositions.flatMap { pos =>
      directions.map(pos + _).filter(pos => pos.positive && !isWall(pos) && !mazeSteps.contains(pos))
    }.distinct
    steps += 1
    mazeSteps ++= currentPositions.map(_ -> steps)
  }

  printDayPart(1, mazeSteps(target))
  printDayPart(2, mazeSteps.count(_._2 <= 50))

  private def isWall(pos: Pos): Boolean = {
    def countBinary(i: Int): Int = i match {
      case 0|1 => i
      case _ => countBinary(i/2) + i%2
    }

    val Pos(x, y) = pos
    countBinary(x*x + 3*x + 2*x*y + y + y*y + favoriteNumber) % 2 == 1
  }
}

package adventofcode.y2016

import scala.collection.parallel.CollectionConverters._

object Day24 extends Year2016 {
  override val day = 24

  private val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))

  private val maze = input.getLines().zipWithIndex.flatMap { line =>
    line._1.zipWithIndex.map(c => (c._2, line._2) -> c._1)
  }.toMap

  private val start = Pos(maze.find(_._2 == '0').get._1)
  private val targets = maze.flatMap {
    case (pos, char) if char >= '1' && char <= '9' => Some(Pos(pos))
    case _ => None
  }.toList

  private val dists = (start :: targets).combinations(2).toList.par.flatMap { way =>
    val steps = way.head.dist(way.last)
    List(way -> steps, way.reverse -> steps)
  }.toMap

  private val paths = targets.permutations.map { path =>
    ((start :: path).sliding(2).map(dists(_)).sum, path.last)
  }.toList

  printDayPart(1, paths.map(_._1).min)

  private val backPaths = paths.map(path => path._1 + dists(List(path._2, start)))

  printDayPart(2, backPaths.min)

  private case class Pos(x: Int, y: Int) {
    def dist(to: Pos): Int = {
      var steps = 0
      var currentPositions = List(this)
      var visited = List(this)

      while(!visited.contains(to)) {
        currentPositions = currentPositions.flatMap { pos =>
          directions.map(pos + _).filter(pos => !pos.isWall && !visited.contains(pos))
        }.distinct
        steps += 1
        visited :::= currentPositions
      }

      steps
    }

    def +(direction: (Int, Int)): Pos = Pos(x + direction._1, y + direction._2)
    def isWall: Boolean = maze(x, y) == '#'
  }
  private object Pos {
    def apply(pos: (Int, Int)): Pos = Pos(pos._1, pos._2)
  }
}

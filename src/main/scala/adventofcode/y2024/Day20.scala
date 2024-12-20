package adventofcode.y2024

import adventofcode.Logging
import adventofcode.common.pos.Pos
import adventofcode.common.search.Dijkstra

import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day20 extends Year2024 {
  override val day = 20

  override def runDay(input: BufferedSource): Unit = {
    val (map, start, end) = Pos.parseMapStartEnd(input.getLines().takeWhile(_.nonEmpty))
    val walls = map.filter(_._2 == '#').keySet

    val course = Dijkstra(start, _ == end, next(map))._2.reverse

    val cheats = findCheatPositions(walls).par.map { cheat =>
      val indexes = cheat.directions.filterNot(walls.contains).map(course.indexOf)
      cheat -> (indexes.max - indexes.min - 2)
    }

    // logCheats(cheats.seq)
    printDayPart(1, cheats.count(_._2 >= 100), "%s cheats would save at least 100 picoseconds")

    val cheats2 = course.par.flatMap { from =>
      findTargets(from, course.dropWhile(_ != from).tail.toSet).flatMap { to =>
        val distance = from.distance(to)
        val steps = course.indexOf(to) - course.indexOf(from) - distance
        if (steps <= 0) {
          None
        } else {
          // println(s"Cheating ($cheat) from $from to $to in $steps steps")
          Some((from, to) -> steps)
        }
      }
    }

    // logCheats(cheats2.seq, 50)
    printDayPart(2, cheats2.count(_._2 >= 100), "%s cheats up to 20 picoseconds would save at least 100 picoseconds")
  }

  private def findCheatPositions(walls: Set[Pos]): List[Pos] = {
    val min = Pos(1, 1)
    val max = walls.max - Pos(1, 1)
    walls.filter { pos => pos.isInBounds(min, max) && pos.directions.count(!walls.contains(_)) >= 2 }.toList
  }

  private def findTargets(start: Pos, track: Set[Pos]): Set[Pos] = {
    track.filter(pos => pos != start && pos.distance(start) <= 20)
  }

  private def next(map: Map[Pos, Char])(pos: Pos): List[(Int, Pos)] =
    pos.directions.filter(map(_) != '#').map((1, _))

  private def logCheats[T](cheats: Seq[(T, Int)], min: Int = 0): Unit = if (Logging.debug)
    cheats.groupBy(_._2).toList.sortBy(_._1).foreach { case (steps, cheats) =>
      if (steps >= min) println(s"There are ${cheats.size} cheats that save $steps picoseconds.")
    }
}

package adventofcode.y2019

import adventofcode.common.pos.Pos

import scala.annotation.tailrec

object Day24 extends Year2019 {
  override val day = 24

  private val map = Pos.parseMap(input.getLines(), char => char == '#')

  private val bugsInFirstRepeatedState = findRepeat(map).groupBy(_._2)(true).keys
  private val biodiversityPoints = bugsInFirstRepeatedState.map { pos => math.pow(2, pos.x + pos.y * 5).toLong }

  printDayPart(1, biodiversityPoints.sum, "biodiversity rating for first repeated state: %s")

  private val levelsAfter200Minutes = (1 to 200).foldLeft(Map(0 -> (map - Pos(2, 2)))) { (levels, minute) =>
    val depth = (minute + 1) / 2
    (-depth to depth).map { level =>
      level -> (
        if (levels.contains(level)) {
          transform(levels(level), { pos: Pos => countNeighborsWithLevels(levels, pos, level) })
        } else {
          val newLevel = (for {
            x <- 0 until 5
            y <- 0 until 5
            if x != 2 || y != 2
          } yield Pos(x, y) -> false).toMap
          transform(newLevel, { pos: Pos => countNeighborsWithLevels(levels + (level -> newLevel), pos, level) })
        })
    }.toMap
  }

  printDayPart(2, levelsAfter200Minutes.flatMap(_._2.values).count(identity), "there are %s bugs after 200 minutes")

  @tailrec
  private def findRepeat(map: Map[Pos, Boolean], seenStates: Set[Map[Pos, Boolean]] = Set.empty): Map[Pos, Boolean] = {
    val nextMap = transform(map, { pos: Pos => countNeighbors(map, pos) })

    if (seenStates.contains(nextMap))
      nextMap
    else
      findRepeat(nextMap, seenStates + nextMap)
  }

  private def transform(map: Map[Pos, Boolean], countFunction: Pos => Int) =
    map.map {
      case (pos, true) => pos -> (countFunction(pos) == 1)
      case (pos, false) => pos -> Set(1, 2).contains(countFunction(pos))
    }

  private def countNeighbors(map: Map[Pos, Boolean], pos: Pos) =
    pos.directions.map(map.getOrElse(_, false)).count(identity)

  private def countNeighborsWithLevels(levels: Map[Int, Map[Pos, Boolean]], pos: Pos, level: Int) =
    pos.directions.flatMap(newPos => levels(level).get(newPos) match {
      case Some(bug) => Seq(bug) // on same level
      case None => // not on same level
        if (newPos == Pos(2, 2)) {
          if (levels.contains(level + 1)) {
            val innerLevel = levels(level + 1)
            pos match {
              case Pos(1, 2) => innerLevel.filter(_._1.x == 0).values
              case Pos(2, 1) => innerLevel.filter(_._1.y == 0).values
              case Pos(3, 2) => innerLevel.filter(_._1.x == 4).values
              case Pos(2, 3) => innerLevel.filter(_._1.y == 4).values
              case _ => throw new IllegalStateException(s"$pos isn't a neighbor of an inner level")
            }
          } else Seq(false)
        } else {
          if (levels.contains(level - 1)) {
            val outerLevel = levels(level - 1)
            newPos match {
              case Pos(-1, _) => Seq(outerLevel(Pos(1, 2)))
              case Pos(_, -1) => Seq(outerLevel(Pos(2, 1)))
              case Pos(5, _) => Seq(outerLevel(Pos(3, 2)))
              case Pos(_, 5) => Seq(outerLevel(Pos(2, 3)))
              case _ => throw new IllegalStateException(s"$newPos isn't right outside of a level")
            }
          } else Seq(false)
        }
    }).count(identity)
}

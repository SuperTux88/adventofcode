package adventofcode.y2019

import adventofcode.Logging
import adventofcode.common.Pos

import scala.annotation.tailrec
import scala.collection.MapView

object Day15 extends Year2019 {
  override val day = 15

  // special order: north (1), south (2), west (3), and east (4)
  private val directions = List((1, (0, -1)), (2, (0, 1)), (3, (-1, 0)), (4, (1, 0)))

  private val intCode = new IntCode(inputString)

  private val exploreResult = exploreMap(List((Pos.zero, intCode)))
  private val (oxygenSystem, (_, steps)) = exploreResult.find(_._2._1 == 2).get

  if (Logging.debug) printMap(exploreResult.view.mapValues(_._1))

  printDayPart(1, steps, "fewest steps to oxygent system: %s")

  private val freeSpace = exploreResult.filter(_._2._1 == 1).keys.toList
  printDayPart(2, fillWithOxygen(List(oxygenSystem), freeSpace), "minutes to fill area: %s")

  @tailrec
  private def exploreMap(current: List[(Pos, IntCode)], visited: Map[Pos, (Int, Int)] = Map(Pos.zero -> (1, 0)), steps: Int = 1): Map[Pos, (Int, Int)] =
    if (current.isEmpty) {
      visited
    } else {
      val nextSteps = current.flatMap {
        case (pos, droid) =>
          directions.filterNot(dir => visited.contains(pos + dir._2)).map { dir =>
            val res = droid.run(dir._1)
            DroidState(res, pos + dir._2, res.output.next.toInt)
          }
      }

      val newPositions = nextSteps.filterNot(_.output == 0).map(step => (step.pos, step.intCode))
      val newVisited = nextSteps.map(step => step.pos -> (step.output, steps))
      exploreMap(newPositions, visited ++ newVisited, steps + 1)
    }

  @tailrec
  private def fillWithOxygen(current: List[Pos], todo: List[Pos], minutes: Int = 0): Int =
    if (todo.isEmpty) {
      minutes
    } else {
      val nextSteps = current.flatMap { pos => Pos.directions.map(pos + _).filter(todo.contains) }
      fillWithOxygen(nextSteps, todo.filterNot(nextSteps.contains), minutes + 1)
    }

  private def printMap(map: MapView[Pos, Int]): Unit =
    (map.keys.map(_.y).min to map.keys.map(_.y).max).foreach(y =>
      println((map.keys.map(_.x).min to map.keys.map(_.x).max).map(x => map.getOrElse(Pos(x, y), 0) match {
        case 0 => "█"
        case 1 => " "
        case 2 => "O"
      }).mkString)
    )

  private case class DroidState(intCode: IntCode, pos: Pos, output: Int)
}

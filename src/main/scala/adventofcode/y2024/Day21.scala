package adventofcode.y2024

import adventofcode.common.MapImplicits.{IntegralMapImplicits, MapImplicits}
import adventofcode.common.pos.{Direction, Pos}

import scala.io.BufferedSource

object Day21 extends Year2024 {
  override val day = 21

  private val NUM_PAD = Pos.parseMap(
    """789
      |456
      |123
      | 0A""".stripMargin.linesIterator,
    identity
  )
  private val DIR_PAD = Pos.parseMap(
    """ ^A
      |<v>""".stripMargin.linesIterator,
    identity
  )

  override def runDay(input: BufferedSource): Unit = {
    val numbers = input.getLines().takeWhile(_.nonEmpty).toList
    val numbersInt = numbers.map(_.dropRight(1).toInt)

    val numPaths = numbers.map { number =>
      ('A' :: number.toList).sliding(2).foldLeft(List.empty[Char]) { (path, pairs) =>
        (pairs: @unchecked) match {
          case List(from, to) => path ::: findPathForStep(from, to, NUM_PAD)
        }
      }
    }

    val directionSubPaths = ('A' :: Direction.arrowDirections).combinations(2).flatMap { pairs =>
      (pairs: @unchecked) match {
        case List(from, to) =>
          Seq((from, to) -> findPathForStep(from, to, DIR_PAD), (to, from) -> findPathForStep(to, from, DIR_PAD))
      }
    }.toMap

    val routes = numPaths.map(getDirPadRoutes(directionSubPaths))
    val complexities = getComplexities(numbersInt, routes)
    printDayPart(1, complexities.sum, "Complexity of the 5 codes: %s")

    val routes25 = numPaths.map(getDirPadRoutes(directionSubPaths, 25))
    val complexities25 = getComplexities(numbersInt, routes25)
    printDayPart(2, complexities25.sum, "Complexity with 26 robots: %s")
  }

  private def findPathForStep(source: Char, target: Char, pad: Map[Pos, Char]): List[Char] = {
    val from = pad.findKeyByValue(source).get
    val to = pad.findKeyByValue(target).get
    val diff = to - from

    val horizontal = if (diff.x < 0) List.fill(diff.x.abs)('<') else List.fill(diff.x)('>')
    val vertical = if (diff.y < 0) List.fill(diff.y.abs)('^') else List.fill(diff.y)('v')

    if (
      diff.x < 0 && pad(from.copy(x = to.x)) != ' ' // if needing to go left, try going there first if possible
        || pad(from.copy(y = to.y)) == ' ' // or if going vertical first would end on blank key
    ) {
      horizontal ::: vertical ::: List('A')
    } else { // else go vertical first
      vertical ::: horizontal ::: List('A')
    }
  }

  private def getDirPadRoutes(directions: Map[(Char, Char), List[Char]], levels: Int = 2)
                             (path: List[Char]): Map[List[Char], Long] =
    (1 to levels).foldLeft(Map(path -> 1L)) { (counter, _) =>
      counter.foldLeft(Map.empty[List[Char], Long]) { case (newCounter, (subPath, count)) =>
        getRoutesCount(subPath, directions).foldLeft(newCounter) { case (newCounter, (newSubPath, newCount)) =>
          newCounter.changeBy(newSubPath, count * newCount)
        }
      }
    }

  private def getRoutesCount(path: List[Char], directions: Map[(Char, Char), List[Char]]): Map[List[Char], Int] =
    ('A' :: path).sliding(2).foldLeft(Map.empty[List[Char], Int]) { (counter, pairs) =>
      (pairs: @unchecked) match {
        case List(from, to) if from == to => counter.changeBy(List('A'), 1)
        case List(from, to) => counter.changeBy(directions(from -> to), 1)
      }
    }

  private def getComplexities(ints: List[Int], paths: List[Map[List[Char], Long]]): List[Long] =
    ints.zip(paths).map { case (value, path) =>
      val length = path.map((subPath, count) => subPath.size * count).sum
      printDebug(s"$length * $value")
      value * length
    }
}

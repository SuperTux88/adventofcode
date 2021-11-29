package adventofcode.y2018

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day12 extends Year2018 {
  override val day = 12

  private val ReplacementRE = """([.#]{5}) => ([.#])""".r

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()
    val initialState = lines.next().split(" ")(2).map(_ == '#').toList

    val transformations = lines.drop(1).map {
      case ReplacementRE(from, to) => from.map(_ == '#').toList -> (to == "#")
    }.toMap

    val after20Generations = (1 to 20).foldLeft(List.fill(10)(false) ::: initialState ::: List.fill(10)(false)) { (state, _) =>
      transformPots(transformations, state)
    }

    printDayPart(1, sumPots(after20Generations))

    val (sum, steps, pattern) = findPattern(transformations, after20Generations, 20)

    printDayPart(2, sum + (50000000000L - steps) * pattern)
  }

  private def transformPots(transformations: Map[List[Boolean], Boolean], pots: List[Boolean]) =
    (false :: false :: pots ::: List.fill(4)(false)).sliding(5).map { slice =>
      transformations.getOrElse(slice, false)
    }.toList

  private def sumPots(pots: List[Boolean], offset: Int = -10) =
    pots.zipWithIndex.map {
      case (true, index) => index + offset
      case (false, _)    => 0
    }.sum

  @tailrec
  private def findPattern(transformations: Map[List[Boolean], Boolean], pots: List[Boolean], steps: Int): (Int, Int, Int) = {
    val nextPots = transformPots(transformations, pots)

    if (trimEmptyPots(nextPots) == trimEmptyPots(pots)) {
      val sum = sumPots(pots)
      (sum, steps, sumPots(nextPots) - sum)
    } else {
      findPattern(transformations, nextPots, steps + 1)
    }
  }

  private def trimEmptyPots(pots: List[Boolean]) = pots.dropWhile(!_).reverse.dropWhile(!_).reverse
}

package adventofcode.y2018

import scala.annotation.tailrec

object Day12 extends Year2018 {
  override val day = 12

  val ReplacementRE = """([.#]{5}) => ([.#])""".r

  val lines = input.getLines
  var initialState = lines.next().split(" ")(2).map(_ == '#').toList

  val transformations = lines.drop(1).map {
    case ReplacementRE(from, to) => from.map(_ == '#').toList -> (to == "#")
  }.toMap

  val after20Generations = (1 to 20).foldLeft(List.fill(10)(false) ::: initialState ::: List.fill(10)(false)) { (state, _) =>
    transformPots(state)
  }

  printDayPart(1, sumPots(after20Generations))

  val (sum, steps, pattern) = findPattern(after20Generations, 20)

  printDayPart(2, sum + (50000000000L - steps) * pattern)

  def transformPots(pots: List[Boolean]) =
    (false :: false :: pots ::: List.fill(4)(false)).sliding(5).map { slice =>
      transformations.getOrElse(slice, false)
    }.toList

  def sumPots(pots: List[Boolean], offset: Int = -10) =
    pots.zipWithIndex.map {
      case (true, index) => index + offset
      case (false, _)    => 0
    }.sum

  @tailrec
  def findPattern(pots: List[Boolean], steps: Int, pattern: Int = 0): (Int, Int, Int) = {
    val nextPots = transformPots(pots)

    if (trimEmptyPots(nextPots) == trimEmptyPots(pots)) {
      val sum = sumPots(pots)
      (sum, steps, sumPots(nextPots) - sum)
    } else {
      findPattern(nextPots, steps + 1)
    }
  }

  def trimEmptyPots(pots: List[Boolean]) = pots.dropWhile(!_).reverse.dropWhile(!_).reverse
}

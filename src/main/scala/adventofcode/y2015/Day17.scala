package adventofcode.y2015

object Day17 extends Year2015 {
  override val day: Int = 17

  val containers = input.getLines().map(_.toInt).toList

  val allCombinations = (1 to containers.size).flatMap(containers.zipWithIndex.combinations)
  val fullCombinations = allCombinations.filter(_.map(_._1).sum == 150)

  printDayPart(1, fullCombinations.size)
  printDayPart(2, fullCombinations.count(_.size == fullCombinations.head.size))
}

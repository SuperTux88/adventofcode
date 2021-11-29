package adventofcode.y2018

object Day5 extends Year2018 {
  override val day = 5

  override def runDay(input: String): Unit = {
    val polymerAfterPart1 = react(input)
    printDayPart(1, polymerAfterPart1.length)

    val shortest = ('a' to 'z').map { c => react(polymerAfterPart1.filterNot(_.toLower == c)).length }.min
    printDayPart(2, shortest)
  }

  private def react(polymer: String) = {
    polymer.foldLeft(List[Char]())({
      case (lastUnit :: otherUnits, unit) if lastUnit != unit && lastUnit.toLower == unit.toLower => otherUnits
      case (otherUnits, unit) => unit :: otherUnits
    }).reverse.mkString
  }
}

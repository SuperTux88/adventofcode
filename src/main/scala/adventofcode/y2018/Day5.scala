package adventofcode.y2018

object Day5 extends Year2018 {
  override val day = 5

  def react(polymer: String) = {
    polymer.foldLeft(List[Char]())({
      case (lastUnit :: otherUnits, unit) if lastUnit != unit && lastUnit.toLower == unit.toLower => otherUnits
      case (otherUnits, unit) => unit :: otherUnits
    }).reverse.mkString
  }

  val polymerAfterPart1 = react(input.mkString)
  printDayPart(1, polymerAfterPart1.length)

  val shortest = ('a' to 'z').map { c => react(polymerAfterPart1.filterNot(_.toLower == c)).length }.min
  printDayPart(2, shortest)
}

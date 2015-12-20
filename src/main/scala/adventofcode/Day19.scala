package adventofcode

object Day19 extends DayApp {
  override val day: Int = 19

  val ReplacementRE = """(\w+) => (\w+)""".r

  val lines = input.getLines()
  val replacements = lines.takeWhile(_.nonEmpty).map {
    case ReplacementRE(from, to) => from -> to
  }.toList
  val medicine = lines.next()

  val distinctMolecules = replacements.flatMap { case (from, to) =>
    medicine.sliding(from.length).zipWithIndex.collect {
      case (`from`, index) => medicine.take(index) + to + medicine.drop(index + from.length)
    }
  }.distinct

  printDayPart(1, distinctMolecules.size)
}

package adventofcode.y2015

import scala.io.BufferedSource

object Day19 extends Year2015 {
  override val day: Int = 19

  private val ReplacementRE = """(\w+) => (\w+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()
    val replacements = lines.takeWhile(_.nonEmpty).map {
      case ReplacementRE(from, to) => from -> to
    }.toList
    val medicine = lines.next()

    val distinctMolecules = replacements.flatMap {
      case (from, to) => transition(from, to, medicine)
    }.distinct

    var counter = 0
    var molecule = medicine

    printDayPart(1, distinctMolecules.size)

    do {
      molecule = replacements.flatMap {
        case (from, to) => transition(to, from, molecule)
      }.distinct.minBy(_.length)
      counter += 1
    } while (molecule != "e") // why does this work? :D

    printDayPart(2, counter)
  }

  private def transition(from: String, to: String, molecule: String) =
    molecule.toSeq.sliding(from.length).map(_.unwrap).zipWithIndex.collect {
      case (`from`, x) => molecule.take(x) + to + molecule.drop(x + from.length)
    }
}

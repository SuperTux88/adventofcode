package adventofcode.y2021

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day3 extends Year2021 {
  override val day = 3

  override def runDay(input: BufferedSource): Unit = {
    val report = input.getLines().takeWhile(_.nonEmpty).map(_.map(_ == '1')).toSeq

    val gammaRate = report.transpose.map(mostCommonBit)
    val epsilonRate = gammaRate.map(!_)

    printDayPart(1, toInt(gammaRate) * toInt(epsilonRate), "power consumption of the submarine: %s")

    val oxygenGeneratorRating = filterBinary(report, mostCommonBit)
    val co2ScrubberRating = filterBinary(report, !mostCommonBit(_))

    printDayPart(2, toInt(oxygenGeneratorRating) * toInt(co2ScrubberRating),
      "life support rating of the submarine: %s")
  }

  private def mostCommonBit(bits: Seq[Boolean]) = bits.count(identity) * 2 >= bits.size

  private def toInt(bits: Seq[Boolean]) = Integer.parseInt(bits.map(if _ then '1' else '0').mkString, 2)

  @tailrec
  private def filterBinary(binaries: Seq[Seq[Boolean]], bitCriteria: Seq[Boolean] => Boolean, index: Int = 0): Seq[Boolean] = {
    val filterValue = bitCriteria(binaries.map(_(index)))
    binaries.filter(_(index) == filterValue) match {
      case Seq(result) => result
      case filteredBinaries => filterBinary(filteredBinaries, bitCriteria, index + 1)
    }
  }
}

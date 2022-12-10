package adventofcode.y2022

import adventofcode.Logging
import adventofcode.common.pos.Pos
import adventofcode.common.{NumberHelper, OCR}

import scala.io.BufferedSource

object Day10 extends Year2022 {
  override val day = 10

  private val AddxRE = """addx ([-\d]+)""".r
  private val NoopRE = """noop""".r

  private val LINE_LENGTH = 40
  private val CENTER_CYCLES = Iterator.iterate(20)(_ + LINE_LENGTH).take(6).toList

  override def runDay(input: BufferedSource): Unit = {
    val signalStrengths = input.getLines().foldLeft(List(1)) {
      case (values, AddxRE(x)) => values.head + x.toInt :: values.head :: values
      case (values, NoopRE()) => values.head :: values
      case (_, line) => throw new MatchError(s"Invalid line: $line")
    }.drop(1).reverse

    val centerSignalStrengths = CENTER_CYCLES.map(c => signalStrengths(c - 1) * c)
    printDayPart(1, centerSignalStrengths.sum, "Sum of the six signal strengths: %s")

    val lines = signalStrengths.zipWithIndex.map {
      case (x, c) => NumberHelper.isInRange(x, c % LINE_LENGTH - 1, c % LINE_LENGTH + 1)
    }.grouped(40).toSeq

    val crt = OCR.convertToMap(lines, if _ then 1 else 0)
    if (Logging.debug) OCR.printImage(crt)

    printDayPart(2, OCR.readMessage(crt, 8, Pos(5, 6)), "Message on the CRT: %s")
  }
}

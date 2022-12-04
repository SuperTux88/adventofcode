package adventofcode.y2022

import scala.io.BufferedSource

object Day4 extends Year2022 {
  override val day = 4

  private val RangeRE = """(\d+)-(\d+),(\d+)-(\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val ranges = input.getLines().map {
      case RangeRE(a, b, c, d) => (a.toInt to b.toInt, c.toInt to d.toInt)
    }.toSeq

    val countContaining = ranges.count(r => containsRange(r._1, r._2) || containsRange(r._2, r._1))
    printDayPart(1, countContaining, "Number of ranges which fully contain the other: %s")

    val countOverlapping = ranges.count(r => overlapsRange(r._1, r._2))
    printDayPart(2, countOverlapping, "Number of ranges which overlap: %s")
  }

  private def containsRange(r1: Range, r2: Range) = r1.contains(r2.min) && r1.contains(r2.max)
  private def overlapsRange(r1: Range, r2: Range) = r1.contains(r2.min) || r2.contains(r1.min)
}

package adventofcode.y2021

import scala.io.BufferedSource

object Day8 extends Year2021 {
  override val day = 8

  private val UNIQUE_NUMBERS = Set(2, 3, 4, 7)

  override def runDay(input: BufferedSource): Unit = {
    val entries = input.getLines().takeWhile(_.nonEmpty).map { line =>
      val Array(patterns, output) = line.split(" \\| ").map(_.split(" ").map(_.toSet))
      Entry(patterns.toSet, output.toSeq)
    }.toList

    val countUniqueDigits = entries.flatMap(_.output.map(_.size)).count(UNIQUE_NUMBERS.contains)
    printDayPart(1, countUniqueDigits, "number of unique numbers in outputs: %s")
    printDayPart(2, entries.map(_.getOutputValue).sum, "sum of output values: %s")
  }

  private case class Entry(patterns: Set[Set[Char]], output: Seq[Set[Char]]) {
    def getOutputValue: Int = {
      val digitMap = getDigitMap
      output.map(digitMap).reduceLeft(_ * 10 + _)
    }

    private def getDigitMap = {
      val one = patterns.find(_.size == 2).get
      val four = patterns.find(_.size == 4).get
      val seven = patterns.find(_.size == 3).get
      val eight = patterns.find(_.size == 7).get

      val fiveSegmentDigits = patterns.filter(_.size == 5)
      val two = fiveSegmentDigits.find(_.diff(four).size == 3).get
      val three = fiveSegmentDigits.find(_.diff(seven).size == 2).get
      val five = (fiveSegmentDigits - two - three).head

      val sixSegmentDigits = patterns.filter(_.size == 6)
      val six = sixSegmentDigits.find(_.diff(one).size == 5).get
      val nine = sixSegmentDigits.find(_.diff(four).size == 2).get
      val zero = (sixSegmentDigits - six - nine).head

      Map(zero -> 0, one -> 1, two -> 2, three -> 3, four -> 4, five -> 5, six -> 6, seven -> 7, eight -> 8, nine -> 9)
    }
  }
}

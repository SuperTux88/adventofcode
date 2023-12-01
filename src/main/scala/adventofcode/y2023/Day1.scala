package adventofcode.y2023

import scala.io.BufferedSource

object Day1 extends Year2023 {
  override val day = 1

  private val DigitsRE = """(\d)""".r
  private val DIGITS = Map(
    "zero" -> "0",
    "one" -> "1",
    "two" -> "2",
    "three" -> "3",
    "four" -> "4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9"
  )
  private val WORDS = DIGITS.keySet
  private val SEARCH = WORDS ++ DIGITS.values

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines().takeWhile(_.nonEmpty).toSeq

    val numbers = lines.map(getValue(_, line => DigitsRE.findAllIn(line).toList))
    printDayPart(1, numbers.sum, "Sum of calibration values: %s")

    val numbersWithWords = lines.map(getValue(_, getMatchesWithWords))
    printDayPart(2, numbersWithWords.sum, "Sum of calibration values with words: %s")
  }

  private def getValue(line: String, matches: String => List[String]): Int = {
    val matchResults = matches(line)
    (matchResults.head + matchResults.last).toInt
  }

  private def getMatchesWithWords(line: String): List[String] = {
    SEARCH.flatMap(search => {
      val firstIndex = line.indexOf(search)
      val lastIndex = line.lastIndexOf(search)
      val value = DIGITS.getOrElse(search, search)
      Seq((firstIndex, value), (lastIndex, value))
    }).filter(_._1 > -1).toList.sortBy(_._1).map(_._2)
  }
}

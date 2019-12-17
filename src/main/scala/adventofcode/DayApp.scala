package adventofcode

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.io.AnsiColor.{GREEN, RED, RESET}

trait DayApp extends App {
  val day: Int
  val year: Int

  def input: BufferedSource = Source.fromInputStream(getClass.getResourceAsStream(s"/input/$year/day$day.txt"))

  def printDayPart(part: Int, solution: Long, formatString: String = "%s"): Unit =
    printDayPart(part, solution.toString, formatString)
  def printDayPart(part: Int, solution: String, formatString: String): Unit = {
    if (Logging.results) {
      println(s"Day $day - Part $part: ${String.format(formatString, s"$GREEN$solution$RESET")}")
      Solutions.check(day, year, part, solution)
    }
  }

  def printDebug(text: String): Unit = {
    if (Logging.debug) println(s"DEBUG: $text")
  }
}

object Logging {
  var debug = true
  var results = true
}

object Solutions {
  def check(day: Int, year: Int, part: Int, solution: String): Unit = {
    val correctSolution = solutions(year)((day - 1) * 2 + part - 1)
    if (solution != correctSolution)
      println(s"$RED$solution is wrong, the expected solution is: $correctSolution$RESET")
  }

  private val solutionsMap = mutable.Map[Int, List[String]]()
  private def solutions(year: Int) = solutionsMap.getOrElseUpdate(year,
    Source.fromInputStream(getClass.getResourceAsStream(s"/input/$year/solutions.txt")).getLines.toList)
}

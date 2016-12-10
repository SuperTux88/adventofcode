package adventofcode

import scala.collection.mutable
import scala.io.{BufferedSource, Source}

trait DayApp extends App {
  val day: Int
  val year: Int

  def input: BufferedSource = Source.fromInputStream(getClass.getResourceAsStream(s"/input/$year/day$day.txt"))

  def printDayPart(part: Int, solution: Long, formatString: String = "%s"): Unit =
    printDayPart(part, solution.toString, formatString)
  def printDayPart(part: Int, solution: String, formatString: String) {
    if (Logging.results) {
      println(s"Day $day - Part $part: ${String.format(formatString, solution)}")
      Solutions.check(day, year, part, solution)
    }
  }

  def printDebug(text: String) {
    if (Logging.debug) println(s"DEBUG: $text")
  }
}
object Logging {
  var debug = true
  var results = true
}
object Solutions {
  def check(day: Int, year: Int, part: Int, solution: String) {
    val correctSolution = solutions(year)((day-1)*2 + part-1)
    if (solution != correctSolution)
      println(s"$solution is wrong, the expected solution is: $correctSolution")
  }

  private val solutionsMap = mutable.Map[Int, List[String]]()
  private def solutions(year: Int) = solutionsMap.getOrElseUpdate(year,
    Source.fromInputStream(getClass.getResourceAsStream(s"/input/$year/solutions.txt")).getLines.toList)
}

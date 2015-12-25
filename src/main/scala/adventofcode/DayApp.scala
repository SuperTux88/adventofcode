package adventofcode

import scala.io.Source

trait DayApp extends App {
  val day: Int

  def input = Source.fromInputStream(getClass.getResourceAsStream(s"/input/day$day.txt"))

  def printDayPart(part: Int, solution: Int, formatString: String = "%s"): Unit =
    printDayPart(part, solution.toString, formatString)
  def printDayPart(part: Int, solution: String, formatString: String) {
    if (Logging.results) {
      println(s"Day $day - Part $part: ${String.format(formatString, solution)}")
      Solutions.check(day, part, solution)
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
  val solutions = Source.fromInputStream(getClass.getResourceAsStream(s"/input/solutions.txt")).getLines().toList

  def check(day: Int, part: Int, solution: String) {
    val correctSolution = solutions((day-1)*2 + part-1)
    if (solution != correctSolution)
      println(s"$solution is wrong, the expected solution is: $correctSolution")
  }
}

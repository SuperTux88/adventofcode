package adventofcode

import scala.io.Source

trait DayApp extends App {
  val day: Int

  def input = Source.fromInputStream(getClass.getResourceAsStream(s"/input/day$day.txt"))

  def printDayPart(part: Int, result: Int): Unit = printDayPart(part, result.toString)
  def printDayPart(part: Int, text: String) {
    if (Logging.results) println(s"Day $day - Part $part: $text")
  }

  def printDebug(text: String) {
    if (Logging.debug) println(s"DEBUG: $text")
  }
}
object Logging {
  var debug = true
  var results = true
}

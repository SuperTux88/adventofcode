package adventofcode

trait DayApp extends App {
  val day: Int

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

package adventofcode

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.io.AnsiColor.{GREEN, RED, RESET}

trait DayApp {
  val day: Int
  val year: Int

  protected def options: Options = _options
  private[this] var _options: Options = _

  final def main(args: Array[String]): Unit = {
    main(new Options(args.toList))
  }
  final def main(options: Options): Unit = {
    _options = options

    val input = if (options.input.isDefined) {
                  Source.fromFile(options.input.get)
                } else {
                  Source.fromInputStream(getClass.getResourceAsStream(s"/input/$year/day$day.txt"))
                }
    runDay(input)
  }

  def runDay(input: BufferedSource): Unit = {
    runDay(input.mkString.trim)
  }
  def runDay(input: String): Unit = ???

  def printDayPart(part: Int, solution: Long, formatString: String = "%s"): Unit =
    printDayPart(part, solution.toString, formatString)
  def printDayPart(part: Int, solution: String, formatString: String): Unit = {
    if (Logging.results) {
      println(s"$year Day $day - Part $part: ${String.format(formatString, s"$GREEN$solution$RESET")}")
      if (options.input.isEmpty) Solutions.check(year, day, part, solution)
    }
  }

  def printDebug(text: String): Unit = {
    if (Logging.debug) println(s"DEBUG: $text")
  }
}

object Logging {
  var debug = true
  var results = true

  def wrap8BitColor(string: String, color: Int) = s"[38;5;${color}m${string}${RESET}"
  def wrap8BitColor(string: String, color: Int, background: Int) = s"[38;5;${color};48;5;${background}m${string}${RESET}"
}

object Solutions {
  def check(year: Int, day: Int, part: Int, solution: String): Unit = {
    getSolution(year, day, part).foreach { correctSolution =>
      if (solution != correctSolution)
        println(s"$RED$solution is wrong, the expected solution is: $correctSolution$RESET")
    }
  }

  def getSolution(year: Int, day: Int, part: Int): Option[String] =
    solutions(year).lift((day - 1) * 2 + part - 1)

  private val solutionsMap = mutable.Map[Int, List[String]]()
  private def solutions(year: Int) = solutionsMap.getOrElseUpdate(year,
    Source.fromInputStream(getClass.getResourceAsStream(s"/input/$year/solutions.txt")).getLines().toList)
}

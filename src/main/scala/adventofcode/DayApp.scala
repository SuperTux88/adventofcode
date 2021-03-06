package adventofcode

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.io.AnsiColor.{GREEN, RED, RESET}

trait DayApp extends App {
  val day: Int
  val year: Int

  protected lazy val options = new Options(args.toList)

  def input: BufferedSource = if (options.input.isDefined) {
    Source.fromFile(options.input.get)
  } else {
    Source.fromInputStream(getClass.getResourceAsStream(s"/input/$year/day$day.txt"))
  }

  def inputString: String = input.mkString.trim

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

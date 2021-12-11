package adventofcode

import scala.annotation.tailrec

class Options(args: List[String]) {
  def hasOptions: Boolean = options.nonEmpty

  def input: Option[String] = options.get("input").asInstanceOf[Option[String]]

  def year: Option[Int] = options.get("year").asInstanceOf[Option[Int]]
  def day: Option[Int] = options.get("day").asInstanceOf[Option[Int]]
  def all: Boolean = options.getOrElse("all", false).asInstanceOf[Boolean]

  def benchmark: Option[Int] = options.getOrElse("benchmark", None).asInstanceOf[Option[Int]]

  def interactive: Boolean = options.getOrElse("interactive", false).asInstanceOf[Boolean]
  def quiet: Boolean = options.getOrElse("quiet", false).asInstanceOf[Boolean]

  private lazy val options = parseArgs(args)

  @tailrec
  private def parseArgs(args: List[String], map: Map[String, Any] = Map.empty): Map[String, Any] = {
    args match {
      case Nil => map
      case "--input" :: input :: tail => parseArgs(tail, map + ("input" -> input))
      case "--year" :: year :: tail =>
        if (year.forall(_.isDigit)) {
          parseArgs(tail, map + ("year" -> year.toInt))
        } else {
          println(s"Invalid option: --year $year is not a Number!")
          System.exit(1).asInstanceOf[Map[String, Any]]
        }
      case "--day" :: day :: tail =>
        if (!map.contains("all")) {
          if (day.forall(_.isDigit)) {
            parseArgs(tail, map + ("day" -> day.toInt))
          } else {
            println(s"Invalid option: --day $day is not a Number!")
            System.exit(1).asInstanceOf[Map[String, Any]]
          }
        } else {
          println(s"Invalid option: Only one of options --day and --all are allowed!")
          System.exit(1).asInstanceOf[Map[String, Any]]
        }
      case "--all" :: tail =>
        if (!map.contains("day")) {
          parseArgs(tail, map + ("all" -> true))
        } else {
          println(s"Invalid option: Only one of options --day and --all are allowed!")
          System.exit(1).asInstanceOf[Map[String, Any]]
        }
      case "--benchmark" :: runs :: tail if runs.forall(_.isDigit) => parseArgs(tail, map + ("benchmark" -> Some(runs.toInt)))
      case "--benchmark" :: tail => parseArgs(tail, map + ("benchmark" -> Some(-1)))
      case "--interactive" :: tail => parseArgs(tail, map + ("interactive" -> true))
      case "--quiet" :: tail => parseArgs(tail, map + ("quiet" -> true))
      case "--help" :: _ =>
        showHelp()
        System.exit(0).asInstanceOf[Map[String, Any]]
      case option :: _ =>
        println(s"Invalid option: $option")
        println()
        showHelp()
        System.exit(1).asInstanceOf[Map[String, Any]]
    }
  }

  private def showHelp(): Unit =
    println(
      """Available options:
        |--input <file>        Use different input file to run. This only works when selecting a single day.
        |--year <year>         Select year to run.
        |--day <day>           Select day of a year to run.
        |--all                 Select all days of a year to run.
        |--benchmark [<runs>]  Enable benchmark mode. Without value it selects the optimal number of runs for each day automatically.
        |--interactive         Run in interactive mode to solve manually. This is only available for a few days where this makes sense.
        |--quiet               Print less output. With this some of the days run faster in single-day-mode because of less waiting for output.
        |--help                Shows this help. :)
        |""".stripMargin)
}

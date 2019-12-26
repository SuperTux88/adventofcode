package adventofcode

import scala.annotation.tailrec

class Options(args: List[String]) {
  def hasOptions: Boolean = options.nonEmpty

  def input: Option[String] = options.get("input").asInstanceOf[Option[String]]

  def year: Option[String] = options.get("year").asInstanceOf[Option[String]]
  def day: Option[Int] = options.get("day").asInstanceOf[Option[Int]]

  def benchmark: Option[Int] = options.getOrElse("benchmark", None).asInstanceOf[Option[Int]]

  private lazy val options = parseArgs(args)

  @tailrec
  private def parseArgs(args: List[String], map: Map[String, Any] = Map.empty): Map[String, Any] = {
    args match {
      case Nil => map
      case "--input" :: input :: tail => parseArgs(tail, map + ("input" -> input))
      case "--year" :: year :: tail => parseArgs(tail, map + ("year" -> year))
      case "--day" :: day :: tail => parseArgs(tail, map + ("day" -> day.toInt))
      case "--benchmark" :: runs :: tail if runs.forall(_.isDigit) => parseArgs(tail, map + ("benchmark" -> Some(runs.toInt)))
      case "--benchmark" :: tail => parseArgs(tail, map + ("benchmark" -> Some(-1)))
      case option :: tail =>
        println(s"Invalid option $option")
        parseArgs(tail, map)
    }
  }
}

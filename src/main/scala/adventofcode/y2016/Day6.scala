package adventofcode.y2016

object Day6 extends Year2016 {
  override val day: Int = 6

  val sortedMessage = input.getLines.toList.transpose.map(_.groupBy(identity).toSeq.sortBy(_._2.length).map(_._1))

  printDayPart(1, sortedMessage.map(_.last).mkString, "message: %s")
  printDayPart(2, sortedMessage.map(_.head).mkString, "message: %s")
}

package adventofcode.y2016

import scala.io.BufferedSource

object Day6 extends Year2016 {
  override val day: Int = 6

  override def runDay(input: BufferedSource): Unit = {
    val sortedMessage = input.getLines().toList.transpose.map(_.groupBy(identity).toSeq.sortBy(_._2.length).map(_._1))

    printDayPart(1, sortedMessage.map(_.last).mkString, "message: %s")
    printDayPart(2, sortedMessage.map(_.head).mkString, "message: %s")
  }
}

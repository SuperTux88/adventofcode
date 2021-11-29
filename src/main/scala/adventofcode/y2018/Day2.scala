package adventofcode.y2018

import scala.io.BufferedSource

object Day2 extends Year2018 {
  override val day = 2

  override def runDay(input: BufferedSource): Unit = {
    val boxIds = input.getLines().toList

    var twoTimes = 0
    var threeTimes = 0

    boxIds.foreach { id =>
      val count = id.toSeq.groupBy(identity).values.map(_.length).toList
      if (count.contains(2)) twoTimes += 1
      if (count.contains(3)) threeTimes += 1
    }
    printDayPart(1, twoTimes * threeTimes)

    val similarChars = boxIds.combinations(2).collectFirst {
      case List(a, b) if a.zip(b).count(pair => pair._1 != pair._2) == 1 =>
        a.zip(b).filter(pair => pair._1 == pair._2).map(_._1).mkString
    }

    printDayPart(2, similarChars.get, "common part of ID: %s")
  }
}

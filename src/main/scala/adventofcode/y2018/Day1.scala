package adventofcode.y2018

import scala.io.BufferedSource
import scala.util.control.Breaks._

object Day1 extends Year2018 {
  override val day = 1

  override def runDay(input: BufferedSource): Unit = {
    val changes = input.getLines().map(_.toInt).toList

    printDayPart(1, changes.sum)

    var frequency = 0
    var knownFrequencies = Set(0)

    breakable {
      while (true) {
        changes.foreach { change =>
          frequency += change

          if (knownFrequencies.contains(frequency)) {
            printDayPart(2, frequency)
            break()
          }

          knownFrequencies += frequency
        }
      }
    }
  }
}

package adventofcode.y2018

import scala.io.BufferedSource

object Day4 extends Year2018 {
  override val day = 4

  private val EventRE = """\[\d{4}-\d{2}-\d{2} \d{2}:(\d{2})\] ([\w\s#]+)""".r
  private val ShiftRE = """Guard #(\d+) begins shift""".r

  override def runDay(input: BufferedSource): Unit = {
    val guardSleepMinutes = input.getLines().toList.sorted
      .foldLeft(Map[Int, List[Int]]().withDefaultValue(List()), None: Option[Int], None: Option[Int]) { (state, event) =>
        val (guardSleepMinutes, currentGuard, fallenAsleep) = state

        event match {
          case EventRE(minute, text) =>
            text match {
              case ShiftRE(guard) =>
                (guardSleepMinutes, Some(guard.toInt), fallenAsleep)
              case "falls asleep" =>
                (guardSleepMinutes, currentGuard, Some(minute.toInt))
              case "wakes up" =>
                val sleepMinutes = guardSleepMinutes(currentGuard.get) ++ (fallenAsleep.get until minute.toInt).toList
                (guardSleepMinutes + (currentGuard.get -> sleepMinutes), currentGuard, None)
            }
        }
      }._1

    val guardMostAsleep = guardSleepMinutes.toSeq.maxBy(_._2.length)
    val minuteMostAsleep = guardMostAsleep._2.groupBy(identity).view.mapValues(_.size).maxBy(_._2)._1
    printDayPart(1, guardMostAsleep._1 * minuteMostAsleep)

    val guardsMinuteMostAsleep = guardSleepMinutes.view.mapValues(_.groupBy(identity).view.mapValues(_.size).maxBy(_._2)._1)
    val guardMostAsleepSameMinute = guardsMinuteMostAsleep.toSeq.maxBy(_._2)
    printDayPart(2, guardMostAsleepSameMinute._1 * guardMostAsleepSameMinute._2)
  }
}

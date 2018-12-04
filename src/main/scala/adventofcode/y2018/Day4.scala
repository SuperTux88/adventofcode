package adventofcode.y2018

object Day4 extends Year2018 {
  override val day = 4

  val EventRE = """\[\d{4}-\d{2}-\d{2} \d{2}:(\d{2})\] ([\w\s#]+)""".r
  val ShiftRE = """Guard #(\d+) begins shift""".r

  var currentGuard = 0
  var guardSleepMinutes = Map[Int, List[Int]]().withDefaultValue(List())
  var fallenAsleep: Option[Int] = None

  input.getLines().toList.sorted.foreach {
    case EventRE(minute, text) => {
      text match {
        case ShiftRE(guard) =>
          currentGuard = guard.toInt
          fallenAsleep = None
        case "falls asleep" =>
          fallenAsleep = Some(minute.toInt)
        case "wakes up" =>
          val sleepMinutes = (fallenAsleep.get until minute.toInt).toList
          guardSleepMinutes += (currentGuard -> (guardSleepMinutes(currentGuard) ++ sleepMinutes))
          fallenAsleep = None
      }
    }
  }

  val guardMostAsleep = guardSleepMinutes.toSeq.maxBy(_._2.length)
  val minuteMostAsleep = guardMostAsleep._2.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
  printDayPart(1, guardMostAsleep._1 * minuteMostAsleep)

  val guardsMinuteMostAsleep = guardSleepMinutes.mapValues(_.groupBy(identity).mapValues(_.size).maxBy(_._2)._1)
  val guardMostAsleepSameMinute = guardsMinuteMostAsleep.toSeq.maxBy(_._2)
  printDayPart(2, guardMostAsleepSameMinute._1 * guardMostAsleepSameMinute._2)
}

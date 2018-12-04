package adventofcode.y2018

object Day4 extends Year2018 {
  override val day = 4

  val EventRE = """\[\d{4}-\d{2}-\d{2} \d{2}:(\d{2})\] ([\w\s#]+)""".r
  val ShiftRE = """Guard #(\d+) begins shift""".r

  val guardSleepMinutes = input.getLines().toList.sorted
    .foldLeft(Map[Int, List[Int]]().withDefaultValue(List()), None:Option[Int], None:Option[Int]) { (state, event) =>
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
  val minuteMostAsleep = guardMostAsleep._2.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
  printDayPart(1, guardMostAsleep._1 * minuteMostAsleep)

  val guardsMinuteMostAsleep = guardSleepMinutes.mapValues(_.groupBy(identity).mapValues(_.size).maxBy(_._2)._1)
  val guardMostAsleepSameMinute = guardsMinuteMostAsleep.toSeq.maxBy(_._2)
  printDayPart(2, guardMostAsleepSameMinute._1 * guardMostAsleepSameMinute._2)
}

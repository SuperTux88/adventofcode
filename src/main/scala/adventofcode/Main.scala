package adventofcode

object Main extends App {
  val days = List(Day1, Day2, Day3, Day4, Day5, Day6, Day7, Day8)

  Logging.debug = false

  // single run
  //days.foreach { day =>
  //  day.main(args)
  //}

  // benchmark
  Logging.results = false
  days.foreach { day =>
    println(day.getClass.getCanonicalName.dropRight(1))
    val times = (1 to 100).map { pass =>
      val start = System.nanoTime
      day.main(args)
      (System.nanoTime - start).toFloat / 1000 / 1000
    }
    println(f"min: ${times.min}%.3fms | avg: ${times.sum / times.size}%.3fms | max: ${times.max}%.3fms | total: ${times.sum}%.3fms")
    //times.foreach(time => print(f"$time%.3f"))
  }

}

package adventofcode

import scala.io.StdIn

object Main extends App {
  val allDays = List(Day1, Day2, Day3, Day4, Day5, Day6, Day7, Day8, Day9, Day10, Day11, Day12, Day13, Day14, Day15, Day16, Day17, Day18, Day19, Day20, Day21)

  Logging.debug = false

  print(
    """Advent of Code 2015
      |===================
      |
      |  1) run and print result (default)
      |  2) run benchmarks
      |
      |Select: """.stripMargin)

  readInput {
    case "1"|"" => Some(ResultMode)
    case "2" => Some(BenchmarkMode)
    case _ => None
  }.run()

  private def readInput[T](matchInput: String => Option[T]) = {
    var result: Option[T] = None
    while (result.isEmpty) {
      result = matchInput(StdIn.readLine())
      if (result.isEmpty) print("Invalid Input, please try again: ")
    }
    println()
    result.get
  }

  private trait DaySelectorRunnable extends Runnable {
    def run() {
      print("Select day number or \"all\" (default: all): ")

      val daysToRun = readInput {
        case Int(dayNumber) if dayNumber > 0 && dayNumber <= allDays.size => Some(List(allDays(dayNumber -1)))
        case "all"|"" => Some(allDays)
        case _ => None
      }

      runDays(daysToRun)
    }

    def runDays(days: List[DayApp])
  }

  private object ResultMode extends DaySelectorRunnable {
    def runDays(days: List[DayApp]) {
      days.foreach { day =>
        day.main(args)
      }
    }
  }

  private object BenchmarkMode extends DaySelectorRunnable {
    def runDays(days: List[DayApp]) {
      Logging.results = false

      print("Number of runs (default: 100): ")

      val runs = readInput {
        case Int(number) if number > 0 => Some(number)
        case "" => Some(100)
        case _ => None
      }

      days.foreach { day =>
        print(s"${day.getClass.getSimpleName.dropRight(1)}: ")

        val times = (1 to runs).map { pass =>
          val start = System.nanoTime
          day.main(args)
          (System.nanoTime - start).toFloat / 1000 / 1000
        }

        println(f"min: ${times.min}%.3fms | avg: ${times.sum / times.size}%.3fms | max: ${times.max}%.3fms | total: ${times.sum}%.3fms")
        if (util.Properties.propIsSet("benchmark.times"))
          println(times.map(time => f"$time%.3f").mkString(", "))
      }
    }
  }

  private object Int {
    def unapply(s: String): Option[Int] = try {
      Some(s.toInt)
    } catch {
      case _: java.lang.NumberFormatException => None
    }
  }
}

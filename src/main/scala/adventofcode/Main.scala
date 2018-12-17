package adventofcode

import scala.io.StdIn

object Main extends App {
  val allDays2015 = List(y2015.Day1, y2015.Day2, y2015.Day3, y2015.Day4, y2015.Day5, y2015.Day6, y2015.Day7, y2015.Day8, y2015.Day9, y2015.Day10, y2015.Day11, y2015.Day12, y2015.Day13, y2015.Day14, y2015.Day15, y2015.Day16, y2015.Day17, y2015.Day18, y2015.Day19, y2015.Day20, y2015.Day21, y2015.Day22, y2015.Day23, y2015.Day24, y2015.Day25)
  val allDays2016 = List(y2016.Day1, y2016.Day2, y2016.Day3, y2016.Day4, y2016.Day5, y2016.Day6, y2016.Day7, y2016.Day8, y2016.Day9, y2016.Day10, y2016.Day11, y2016.Day12, y2016.Day13, y2016.Day14, y2016.Day15, y2016.Day16, y2016.Day17, y2016.Day18, y2016.Day19, y2016.Day20, y2016.Day21, y2016.Day22, y2016.Day23, y2016.Day24, y2016.Day25)
  val allDays2018 = List(y2018.Day1, y2018.Day2, y2018.Day3, y2018.Day4, y2018.Day5, y2018.Day6, y2018.Day7, y2018.Day8, y2018.Day9, y2018.Day10, y2018.Day11, y2018.Day12, y2018.Day13, y2018.Day14, y2018.Day15, y2018.Day16, y2018.Day17)

  Logging.debug = false

  print(
    """Advent of Code
      |==============
      |
      |Select year:
      |
      |  - 2015
      |  - 2016
      |  - 2018 (default)
      |
      |Year: """.stripMargin)

  val allDays = readInput {
    case "2015" => Some(allDays2015)
    case "2016" => Some(allDays2016)
    case "2018"|"" => Some(allDays2018)
    case _ => None
  }

  print(
    """Select mode:
      |
      |  1) run and print result (default)
      |  2) run benchmarks
      |
      |Mode: """.stripMargin)

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

package adventofcode

import scala.io.StdIn

object Main extends App {
  val allDays2015 = List(y2015.Day1, y2015.Day2, y2015.Day3, y2015.Day4, y2015.Day5, y2015.Day6, y2015.Day7, y2015.Day8, y2015.Day9, y2015.Day10, y2015.Day11, y2015.Day12, y2015.Day13, y2015.Day14, y2015.Day15, y2015.Day16, y2015.Day17, y2015.Day18, y2015.Day19, y2015.Day20, y2015.Day21, y2015.Day22, y2015.Day23, y2015.Day24, y2015.Day25)
  val allDays2016 = List(y2016.Day1, y2016.Day2, y2016.Day3, y2016.Day4, y2016.Day5, y2016.Day6, y2016.Day7, y2016.Day8, y2016.Day9, y2016.Day10, y2016.Day11, y2016.Day12, y2016.Day13, y2016.Day14, y2016.Day15, y2016.Day16, y2016.Day17, y2016.Day18, y2016.Day19, y2016.Day20, y2016.Day21, y2016.Day22, y2016.Day23, y2016.Day24, y2016.Day25)
  val allDays2018 = List(y2018.Day1, y2018.Day2, y2018.Day3, y2018.Day4, y2018.Day5, y2018.Day6, y2018.Day7, y2018.Day8, y2018.Day9, y2018.Day10, y2018.Day11, y2018.Day12, y2018.Day13, y2018.Day14, y2018.Day15, y2018.Day16, y2018.Day17, y2018.Day18, y2018.Day19, y2018.Day20, y2018.Day21, y2018.Day22, y2018.Day23, y2018.Day24, y2018.Day25)
  val allDays2019 = List(y2019.Day1, y2019.Day2, y2019.Day3, y2019.Day4, y2019.Day5, y2019.Day6, y2019.Day7, y2019.Day8, y2019.Day9, y2019.Day10, y2019.Day11, y2019.Day12, y2019.Day13, y2019.Day14, y2019.Day15, y2019.Day16, y2019.Day17, y2019.Day18, y2019.Day19, y2019.Day20, y2019.Day21, y2019.Day22, y2019.Day23, y2019.Day24, y2019.Day25)
  val allDays2020 = List(y2020.Day1, y2020.Day2)

  val years = Map("2015" -> allDays2015, "2016" -> allDays2016, "2018" -> allDays2018, "2019" -> allDays2019, "2020" -> allDays2020)

  private val options = new Options(args.toList)

  println(
    """Advent of Code
      |==============
      |""".stripMargin)

  val allDays = if (options.year.isDefined) {
    years(options.year.get)
  } else {
    print(
      """Select year:
        |
        |  - 2015
        |  - 2016
        |  - 2018
        |  - 2019
        |  - 2020 (default)
        |
        |Year: """.stripMargin)

    readInput {
      case year if years.contains(year) => Some(years(year))
      case "" => Some(allDays2020)
      case _ => None
    }
  }

  if (options.hasOptions) {
    if (options.benchmark.isEmpty) ResultMode.run() else BenchmarkMode.run()
  } else {
    print(
      """Select mode:
        |
        |  1) run and print result (default)
        |  2) run benchmarks
        |
        |Mode: """.stripMargin)

    readInput {
      case "1" | "" => Some(ResultMode)
      case "2" => Some(BenchmarkMode)
      case _ => None
    }.run()
  }

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
    def run(): Unit = {
      val daysToRun = if (options.day.isDefined) {
        List(allDays(options.day.get - 1))
      } else {
        print("Select day number or \"all\" (default: all): ")

        readInput {
          case Int(dayNumber) if dayNumber > 0 && dayNumber <= allDays.size => Some(List(allDays(dayNumber - 1)))
          case "all" | "" =>
            Logging.debug = false
            Some(allDays)
          case _ => None
        }
      }

      runDays(daysToRun)
    }

    def runDays(days: List[DayApp]): Unit
  }

  private object ResultMode extends DaySelectorRunnable {
    def runDays(days: List[DayApp]): Unit = {
      days.foreach { day =>
        day.main(args)
      }
    }
  }

  private object BenchmarkMode extends DaySelectorRunnable {
    private val benchmarkRunsForDays = Map[DayApp, Int](
      y2015.Day1 -> 1000, y2015.Day2 -> 1000, y2015.Day3 -> 1000, y2015.Day4 -> 50, y2015.Day7 -> 1000, y2015.Day8 -> 1000, y2015.Day10 -> 25, y2015.Day11 -> 10, y2015.Day13 -> 10, y2015.Day15 -> 10, y2015.Day16 -> 1000, y2015.Day18 -> 5, y2015.Day20 -> 5, y2015.Day21 -> 1000, y2015.Day22 -> 10, y2015.Day23 -> 1000, y2015.Day25 -> 1000,
      y2016.Day1 -> 1000, y2016.Day2 -> 1000, y2016.Day3 -> 1000, y2016.Day5 -> 5, y2016.Day6 -> 1000, y2016.Day8 -> 1000, y2016.Day9 -> 1000, y2016.Day10 -> 1000, y2016.Day11 -> 3, y2016.Day12 -> 10, y2016.Day13 -> 1000, y2016.Day14 -> 2, y2016.Day15 -> 1000, y2016.Day16 -> 5, y2016.Day18 -> 5, y2016.Day19 -> 1000, y2016.Day20 -> 1000, y2016.Day21 -> 1000, y2016.Day23 -> 1, y2016.Day25 -> 50,
      y2018.Day4 -> 1000, y2018.Day7 -> 1000, y2018.Day8 -> 1000, y2018.Day9 -> 25, y2018.Day10 -> 1000, y2018.Day11 -> 25, y2018.Day14 -> 5, y2018.Day15 -> 10, y2018.Day18 -> 10, y2018.Day19 -> 25, y2018.Day21 -> 1, y2018.Day22 -> 50, y2018.Day24 -> 25,
      y2019.Day1 -> 1000, y2019.Day5 -> 1000, y2019.Day12 -> 25, y2019.Day16 -> 10, y2019.Day18 -> 2, y2019.Day24 -> 25, y2019.Day25 -> 5,
      y2020.Day1 -> 1000, y2020.Day2 -> 1000
    ).withDefaultValue(100)

    def runDays(days: List[DayApp]): Unit = {
      Logging.debug = false
      Logging.results = false

      val selectedRuns = if (options.benchmark.isDefined) {
        options.benchmark.get
      } else {
        print("Number of runs (default: optimal for each day): ")

        readInput {
          case Int(number) if number > 0 => Some(number)
          case "" => Some(-1)
          case _ => None
        }
      }

      days.foreach { day =>
        print(s"${day.getClass.getSimpleName.dropRight(1)}: ")

        import java.lang.management.ManagementFactory
        import com.sun.management.OperatingSystemMXBean

        val osMBean = ManagementFactory.newPlatformMXBeanProxy(ManagementFactory.getPlatformMBeanServer, ManagementFactory.OPERATING_SYSTEM_MXBEAN_NAME, classOf[OperatingSystemMXBean])
        val (nanoBefore, cpuBefore) = (System.nanoTime, osMBean.getProcessCpuTime)

        val runs = if (selectedRuns > 0) selectedRuns else benchmarkRunsForDays(day)
        print(s"runs: $runs")
        val times = (1 to runs).map { pass =>
          val start = System.nanoTime
          day.main(args)
          (System.nanoTime - start).toFloat / 1000 / 1000
        }

        val cpuTime = osMBean.getProcessCpuTime - cpuBefore
        val percent = cpuTime * 100 / (System.nanoTime - nanoBefore)

        import scala.math.Ordering.Float.TotalOrdering
        println(f" | min: ${times.min}%.3fms | avg: ${times.sum / times.size}%.3fms | max: ${times.max}%.3fms | total: ${times.sum}%.3fms | cpu-time: ${cpuTime/1000/1000}%dms | cpu-usage: ${percent}%d%%")
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

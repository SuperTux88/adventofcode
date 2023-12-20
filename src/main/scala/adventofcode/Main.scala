package adventofcode

import adventofcode.common.NumberHelper.isInRange

import scala.io.StdIn

object Main {
  final def main(args: Array[String]): Unit = {
    val options = new Options(args.toList)

    println(
      """Advent of Code
        |==============
        |""".stripMargin)

    val allDays = if (options.year.isDefined) {
      if (AllDays.yearExists(options.year.get)) {
        AllDays.year(options.year.get)
      } else {
        println(s"No solutions for year ${options.year.get}!")
        System.exit(1).asInstanceOf[List[DayApp]]
      }
    } else if (options.all) {
      AllDays.allYears
    } else {
      print(
        """Select year:
          |
          |  - 2015
          |  - 2016
          |  - 2018
          |  - 2019
          |  - 2020
          |  - 2021
          |  - 2022
          |  - 2023 (default)
          |
          |Year: """.stripMargin)

      readInput {
        case Int(year) if AllDays.year(year).nonEmpty => Some(AllDays.year(year))
        case "" => Some(AllDays.year(2023))
        case _ => None
      }
    }

    if (options.hasOptions) {
      if (options.benchmark.isEmpty) ResultMode.run(allDays, options) else BenchmarkMode.run(allDays, options)
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
      }.run(allDays, options)
    }
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

  private trait DaySelectorRunnable {
    def run(allDays: List[DayApp], options: Options): Unit = {
      val daysToRun = if (options.day.isDefined) {
        if (isInRange(options.day.get, 1, allDays.size)) {
          List(allDays(options.day.get - 1))
        } else {
          println(s"No solution for day ${options.day.get}!")
          System.exit(1).asInstanceOf[List[DayApp]]
        }
      } else if (options.all) {
        Logging.debug = false
        allDays
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

      runDays(daysToRun, options)
    }

    def runDays(days: List[DayApp], options: Options): Unit
  }

  private object ResultMode extends DaySelectorRunnable {
    def runDays(days: List[DayApp], options: Options): Unit = {
      days.foreach { day =>
        day.main(options)
      }
    }
  }

  private object BenchmarkMode extends DaySelectorRunnable {
    private val benchmarkRunsForDays = Map[DayApp, Int](
      y2015.Day1 -> 1000, y2015.Day2 -> 1000, y2015.Day3 -> 1000, y2015.Day4 -> 50, y2015.Day7 -> 1000, y2015.Day8 -> 1000, y2015.Day10 -> 25, y2015.Day11 -> 10, y2015.Day13 -> 10, y2015.Day15 -> 10, y2015.Day16 -> 1000, y2015.Day18 -> 25, y2015.Day20 -> 5, y2015.Day21 -> 1000, y2015.Day22 -> 10, y2015.Day23 -> 1000, y2015.Day25 -> 1000,
      y2016.Day1 -> 1000, y2016.Day2 -> 1000, y2016.Day3 -> 1000, y2016.Day5 -> 5, y2016.Day6 -> 1000, y2016.Day8 -> 1000, y2016.Day9 -> 1000, y2016.Day10 -> 1000, y2016.Day11 -> 3, y2016.Day12 -> 10, y2016.Day13 -> 1000, y2016.Day14 -> 2, y2016.Day15 -> 1000, y2016.Day16 -> 5, y2016.Day18 -> 5, y2016.Day19 -> 1000, y2016.Day20 -> 1000, y2016.Day21 -> 1000, y2016.Day23 -> 1, y2016.Day25 -> 50,
      y2018.Day4 -> 1000, y2018.Day7 -> 1000, y2018.Day8 -> 1000, y2018.Day9 -> 25, y2018.Day10 -> 1000, y2018.Day11 -> 25, y2018.Day14 -> 5, y2018.Day15 -> 10, y2018.Day18 -> 10, y2018.Day19 -> 25, y2018.Day21 -> 1, y2018.Day22 -> 50, y2018.Day24 -> 25,
      y2019.Day1 -> 1000, y2019.Day5 -> 1000, y2019.Day12 -> 25, y2019.Day16 -> 10, y2019.Day18 -> 2, y2019.Day24 -> 25, y2019.Day25 -> 5,
      y2020.Day1 -> 1000, y2020.Day2 -> 1000, y2020.Day5 -> 1000, y2020.Day10 -> 1000, y2020.Day11 -> 50, y2020.Day12 -> 1000, y2020.Day13 -> 1000, y2020.Day15 -> 10, y2020.Day20 -> 25, y2020.Day21 -> 1000, y2020.Day22 -> 10, y2020.Day23 -> 25, y2020.Day24 -> 25, y2020.Day25 -> 1000,
      y2021.Day1 -> 1000, y2021.Day2 -> 1000, y2021.Day3 -> 1000, y2021.Day6 -> 1000, y2021.Day7 -> 1000, y2021.Day10 -> 1000, y2021.Day13 -> 1000, y2021.Day14 -> 1000, y2021.Day15 -> 25, y2021.Day16 -> 1000, y2021.Day19 -> 10, y2021.Day21 -> 50, y2021.Day23 -> 10, y2021.Day24 -> 1000, y2021.Day25 -> 25,
      y2022.Day1 -> 1000, y2022.Day2 -> 1000, y2022.Day3 -> 1000, y2022.Day4 -> 1000, y2022.Day5 -> 1000, y2022.Day6 -> 1000, y2022.Day7 -> 1000, y2022.Day10 -> 1000, y2022.Day13 -> 1000, y2022.Day14 -> 10, y2022.Day16 -> 5, y2022.Day17 -> 5, y2022.Day19 -> 20, y2022.Day20 -> 20, y2022.Day21 -> 1000, y2022.Day23 -> 5, y2022.Day24 -> 10, y2022.Day25 -> 1000,
      y2023.Day1 -> 1000, y2023.Day2 -> 1000, y2023.Day4 -> 1000, y2023.Day6 -> 1000, y2023.Day8 -> 1000, y2023.Day14 -> 10, y2023.Day15 -> 1000, y2023.Day16 -> 25, y2023.Day17 -> 3, y2023.Day18 -> 1000, y2023.Day19 -> 1000, y2023.Day20 -> 25
    ).withDefaultValue(100)

    def runDays(days: List[DayApp], options: Options): Unit = {
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
        val times = (1 to runs).map { _ =>
          val start = System.nanoTime
          day.main(options)
          (System.nanoTime - start).toFloat / 1000 / 1000
        }

        val cpuTime = osMBean.getProcessCpuTime - cpuBefore
        val percent = cpuTime * 100 / (System.nanoTime - nanoBefore)

        import scala.math.Ordering.Float.TotalOrdering
        println(f" | min: ${times.min}%.3fms | avg: ${times.sum / times.size}%.3fms | max: ${times.max}%.3fms | total: ${times.sum}%.3fms | cpu-time: ${cpuTime / 1000 / 1000}%dms | cpu-usage: ${percent}%d%%")
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

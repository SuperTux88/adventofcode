package adventofcode.y2024

import scala.io.BufferedSource

object Day2 extends Year2024 {
  override val day = 2

  override def runDay(input: BufferedSource): Unit = {
    val reports = input.getLines().takeWhile(_.nonEmpty).map { line =>
      line.split(" ").map(_.toInt).toSeq
    }.toSeq

    printDayPart(1, reports.count(isSafe), "Safe reports: %s")
    printDayPart(2, reports.count(isSafeWithProblemDampener), "Safe reports with problem dampener: %s")
  }

  private def isSafe(report: Seq[Int]) =
    (isIncreasing(report) || isDecreasing(report)) && betweenOneAndThree(report)

  private def isSafeWithProblemDampener(report: Seq[Int]) =
    report.indices.exists(i => isSafe(report.take(i) ++ report.drop(i + 1)))

  private def isIncreasing(report: Seq[Int]) =
    report.sliding(2).forall { case Seq(a, b) => a < b }

  private def isDecreasing(report: Seq[Int]) =
    report.sliding(2).forall { case Seq(a, b) => a > b }

  private def betweenOneAndThree(report: Seq[Int]) =
    report.sliding(2).forall { case Seq(a, b) =>
      val diff = (a - b).abs
      diff >= 1 && diff <= 3
    }
}

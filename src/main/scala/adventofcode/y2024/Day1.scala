package adventofcode.y2024

import adventofcode.common.IterableImplicits

import scala.io.BufferedSource

object Day1 extends Year2024 {
  override val day = 1

  private val NumbersRE = """(\d+)\s+(\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val (left, right) = input.getLines().takeWhile(_.nonEmpty).map {
      case NumbersRE(l, r) => (l.toInt, r.toInt)
    }.toSeq.unzip

    val distances = left.sorted.zip(right.sorted).map { case (a, b) =>
      (a - b).abs
    }
    printDayPart(1, distances.sum, "Distance between the two lists: %s")

    val rightCounts = right.groupCount(identity)
    val similarity = left.map(l => rightCounts.getOrElse(l, 0) * l)
    printDayPart(2, similarity.sum, "Similarity score of the two lists: %s")
  }
}

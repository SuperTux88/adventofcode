package adventofcode.y2024

import adventofcode.common.IterableImplicits

import scala.io.BufferedSource

object Day1 extends Year2024 {
  override val day = 1

  private val NumbersRE = """(\d+)\s+(\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val (list1, list2) = input.getLines().takeWhile(_.nonEmpty).map {
      case NumbersRE(a, b) => (a.toInt, b.toInt)
    }.toSeq.unzip

    val distances = list1.sorted.zip(list2.sorted).map { case (a, b) =>
      (a - b).abs
    }
    printDayPart(1, distances.sum, "Distance between the two lists: %s")

    val list2Counts = list2.groupCount(identity)
    val similarity = list1.map(x => list2Counts.getOrElse(x, 0) * x)
    printDayPart(2, similarity.sum, "Similarity score of the two lists: %s")
  }
}

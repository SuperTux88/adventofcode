package adventofcode.y2024

import scala.io.BufferedSource

object Day1 extends Year2024 {
  override val day = 1

  private val NumbersRE = """(\d+)\s+(\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val (list1, list2) = input.getLines().takeWhile(_.nonEmpty).foldLeft(List.empty[Int], List.empty[Int]) {
      case (lists, NumbersRE(a, b)) => (a.toInt :: lists._1, b.toInt :: lists._2)
      case (lists, string) => throw new IllegalStateException(s"Cant parse '$string' as two numbers")
    }

    val distances = list1.sorted.zip(list2.sorted).map { case (a, b) =>
      (a - b).abs
    }
    printDayPart(1, distances.sum, "Distance between the two lists: %s")

    val similarity = list1.map(x => list2.count(_ == x) * x)
    printDayPart(2, similarity.sum, "Similarity score of the two lists: %s")
  }
}

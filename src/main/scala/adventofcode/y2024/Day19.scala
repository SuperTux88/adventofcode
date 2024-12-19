package adventofcode.y2024

import scala.collection.mutable
import scala.io.BufferedSource

object Day19 extends Year2024 {
  override val day = 19

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()
    val towels = lines.next().split(", ").toSeq
    val patterns = lines.drop(1).toSeq

    val cache = mutable.Map.empty[String, Long]
    val possible = patterns.map(isPossible(towels, _, cache))

    printDayPart(1, possible.count(_ >= 1), "Number of possible patterns: %s")
    printDayPart(2, possible.sum(), "Sum of different ways of possible pattern: %s")
  }

  private def isPossible(towels: Seq[String], pattern: String, cache: mutable.Map[String, Long]): Long = {
    if (pattern.isEmpty) return 1
    if (cache.contains(pattern)) return cache(pattern)

    val result = towels.filter(pattern.startsWith).map { towel =>
      isPossible(towels, pattern.drop(towel.length), cache)
    }.sum

    cache(pattern) = result
    result
  }
}

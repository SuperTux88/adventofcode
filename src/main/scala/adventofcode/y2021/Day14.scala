package adventofcode.y2021

import scala.io.BufferedSource

object Day14 extends Year2021 {
  override val day = 14

  private val PairRE = """(\w)(\w) -> (\w)""".r

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()
    val polymerTemplate = lines.next().toList
    val pairInsertions = lines.drop(1).takeWhile(_.nonEmpty).map {
      case PairRE(a, b, c) => (a.head, b.head) -> c.head
    }.toMap
    val pairsCounter = polymerTemplate.sliding(2).collect { case List(a, b) => (a, b) }.toSeq
      .groupMapReduce(identity)(_ => 1L)(_ + _)

    val pairsCounter10 = (0 until 10).foldLeft(pairsCounter) { (counter, _) => stepCount(counter, pairInsertions) }
    val elementCounts10 = countElements(pairsCounter10, polymerTemplate).values.toSeq
    printDayPart(1, elementCounts10.max - elementCounts10.min,
      "most common minus least common element after 10 steps: %s")

    val pairsCounter40 = (10 until 40).foldLeft(pairsCounter10) { (counter, _) => stepCount(counter, pairInsertions) }
    val elementCounts40 = countElements(pairsCounter40, polymerTemplate).values.toSeq
    printDayPart(2, elementCounts40.max - elementCounts40.min,
      "most common minus least common element after 40 steps: %s")
  }

  private def stepCount(pairsCounter: Map[(Char, Char), Long], pairInsertions: Map[(Char, Char), Char]) =
    pairsCounter.foldLeft(Map[(Char, Char), Long]().withDefaultValue(0L)) {
      case (counter, (pair@(a, b), countCount)) => {
        val insert = pairInsertions(pair)
        val newA = (a, insert)
        val newB = (insert, b)
        counter.updated(newA, counter(newA) + countCount).updated(newB, counter(newB) + countCount)
      }
    }

  private def countElements(pairsCounter: Map[(Char, Char), Long], polymerTemplate: List[Char]): Map[Char, Long] = {
    val counter = pairsCounter.groupMapReduce(_._1._1)(_._2)(_ + _)
    counter.updated(polymerTemplate.last, counter(polymerTemplate.last) + 1)
  }
}

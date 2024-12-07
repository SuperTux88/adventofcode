package adventofcode.y2024

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day7 extends Year2024 {
  override val day = 7

  private val EquationRE = """(\d+): ([\d ]+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val equations = input.getLines().takeWhile(_.nonEmpty).map {
      case EquationRE(result, numbers) => Equation(result.toLong, numbers.split(" ").toIndexedSeq.map(_.toLong).toList)
    }.toSeq.par

    val possibleResults = equations.filter(_.isSolvable)
    printDayPart(1, possibleResults.map(_.result).sum, "Total calibration result: %s")
    
    val possibleResultsWithConcatenation = equations.filter(_.isSolvableWithConcatenation)
    printDayPart(2, possibleResultsWithConcatenation.map(_.result).sum, "Total calibration result with concatenation: %s")
  }

  private case class Equation(result: Long, numbers: List[Long]) {
    def isSolvable: Boolean =
      getResults(numbers.tail, Seq(numbers.head)).contains(result)
    def isSolvableWithConcatenation: Boolean =
      getResultsWithConcatenation(numbers.tail, Seq(numbers.head)).contains(result)
  }

  @tailrec
  private def getResults(numbers: List[Long], results: Seq[Long]): Seq[Long] =
    numbers match {
      case Nil => results
      case number :: tail =>
        val addition = results.map(_ + number)
        val multiplication = results.map(_ * number)
        getResults(tail, addition ++ multiplication)
    }

  @tailrec
  private def getResultsWithConcatenation(numbers: List[Long], results: Seq[Long]): Seq[Long] =
    numbers match {
      case Nil => results
      case number :: tail =>
        val addition = results.map(_ + number)
        val multiplication = results.map(_ * number)
        val concatenation = results.map(r => s"$r$number".toLong)
        getResultsWithConcatenation(tail, addition ++ multiplication ++ concatenation)
    }
}

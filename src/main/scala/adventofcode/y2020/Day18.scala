package adventofcode.y2020

import scala.annotation.tailrec

object Day18 extends Year2020 {
  override val day = 18

  private val ParenthesesRE = """(.*)\(([\d +*]+)\)(.*)""".r

  private val lines = input.getLines().toList

  printDayPart(1, lines.map(solveLine(_, leftToRight)).sum, "sum of results: %s")
  printDayPart(2, lines.map(solveLine(_, additionsFirst)).sum, "sum of results with precedence: %s")

  @tailrec
  private def solveLine(line: String, precedence: Seq[String] => Long): Long =
    line match {
      case ParenthesesRE(prefix, expression, suffix) =>
        solveLine(prefix + precedence(expression.split(' ').toSeq) + suffix, precedence)
      case expression => precedence(expression.split(' ').toSeq)
    }

  private def leftToRight(expression: Seq[String]) =
    expression.tail.grouped(2).foldLeft(expression.head.toLong) { (result, pair) =>
      pair match {
        case Seq("+", number) => result + number.toLong
        case Seq("*", number) => result * number.toLong
      }
    }

  private def additionsFirst(expression: Seq[String]) =
    solveAdditions(expression).sliding(1, 2).map(_.head.toLong).product

  @tailrec
  private def solveAdditions(expression: Seq[String]): Seq[String] = {
    val nextAddition = expression.indexOf("+")
    if (nextAddition == -1) {
      expression
    } else {
      val prefix = expression.slice(0, nextAddition - 1)
      val sum = (expression(nextAddition - 1).toLong + expression(nextAddition + 1).toLong).toString
      val suffix = expression.slice(nextAddition + 2, expression.length)
      solveAdditions((prefix :+ sum) ++ suffix)
    }
  }
}

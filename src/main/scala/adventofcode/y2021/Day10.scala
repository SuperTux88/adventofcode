package adventofcode.y2021

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day10 extends Year2021 {
  override val day = 10

  private val PAIRS = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines().takeWhile(_.nonEmpty).map(_.toList)
    val (corrupted, incomplete) = lines.map(validateLine(_)).partition(_._1.nonEmpty)

    val errorScores = corrupted.flatMap(_._1).map {
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137
    }

    printDayPart(1, errorScores.sum, "sum of syntax error scores: %s")

    val autocompleteScores = incomplete.map {
      _._2.map {
        case '(' => 1L
        case '[' => 2L
        case '{' => 3L
        case '<' => 4L
      }.reduceLeft(_ * 5 + _)
    }.toVector

    val middleScore = autocompleteScores.sorted.drop(autocompleteScores.length / 2).head
    printDayPart(2, middleScore, "middle score for autocompletion: %s")
  }

  @tailrec
  private def validateLine(line: List[Char], open: List[Char] = List.empty): (Option[Char], List[Char]) =
    line match {
      case head :: remaining =>
        if (PAIRS.contains(head)) {
          validateLine(remaining, head :: open)
        } else {
          val lastOpen :: remainingOpen = open: @unchecked
          if (PAIRS(lastOpen) == head)
            validateLine(remaining, remainingOpen)
          else
            (Some(head), open)
        }
      case Nil => (None, open)
    }
}

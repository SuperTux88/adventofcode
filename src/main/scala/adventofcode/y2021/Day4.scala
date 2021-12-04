package adventofcode.y2021

import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day4 extends Year2021 {
  override val day = 4

  override def runDay(input: String): Unit = {
    val (numbersIn, boardsIn) = input.splitAt(input.indexOf("\n\n"))
    val numbers = numbersIn.split(",").map(_.toInt).toList
    val boards = boardsIn.trim.split("\n\n").map { board =>
      board.linesIterator.zipWithIndex.flatMap { case (line, lineNumber) =>
        line.trim.split(" +").zipWithIndex.map { case (number, numberIndex) =>
          (number.toInt, Pos(numberIndex, lineNumber))
        }
      }.toMap
    }.map(Board(_)).toSeq

    val winnerBoard = play(boards, numbers)
    printDayPart(1, winnerBoard.score, "score of the first winning board: %s")

    val lastWinnerBoard = playAll(boards, numbers)
    printDayPart(2, lastWinnerBoard.score, "score of the last winning board: %s")
  }

  @tailrec
  private def play(boards: Seq[Board], numbers: List[Int]): Board = {
    val (number :: remaining) = numbers
    val newBoards = boards.map(_.mark(number))
    newBoards.find(_.isBingo) match {
      case Some(bingoBoard) => bingoBoard
      case None => play(newBoards, remaining)
    }
  }

  @tailrec
  private def playAll(boards: Seq[Board], numbers: List[Int]): Board = {
    val (number :: remaining) = numbers
    boards.map(_.mark(number)) match {
      case Seq(last) => last
      case newBoards => playAll(newBoards.filterNot(_.isBingo), remaining)
    }
  }

  private case class Board(numbers: Map[Int, Pos], marked: List[Pos] = List.empty) {
    def mark(number: Int) =
      numbers.get(number) match {
        case Some(pos) => copy(marked = pos :: marked)
        case None => this
      }

    def isBingo =
      (0 until 5).exists { index =>
        (0 until 5).forall(x => marked.contains(Pos(x, index))) ||
          (0 until 5).forall(y => marked.contains(Pos(index, y)))
      }

    def score = {
      val lastNumber = numbers.find(_._2 == marked.head).get._1
      val unmarkedValues = numbers.filter(num => !marked.contains(num._2)).map(_._1)
      unmarkedValues.sum * lastNumber
    }
  }
}

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

    val (winnerBoard, lastNumber) = play(boards, numbers)
    printDayPart(1, calculateScore(winnerBoard, lastNumber), "score of the first winning board: %s")

    val (lastWinnerBoard, lastWinningNumber) = playAll(boards, numbers)
    printDayPart(2, calculateScore(lastWinnerBoard, lastWinningNumber), "score of the last winning board: %s")
  }

  @tailrec
  private def play(boards: Seq[Board], numbers: List[Int]): (Board, Int) = {
    val (number :: remaining) = numbers
    val newBoards = boards.map(_.mark(number))
    newBoards.find(_._2) match {
      case Some(bingoBoard, true) => (bingoBoard, number)
      case _ => play(newBoards.map(_._1), remaining)
    }
  }

  @tailrec
  private def playAll(boards: Seq[Board], numbers: List[Int]): (Board, Int) = {
    val (number :: remaining) = numbers
    boards.map(_.mark(number)) match {
      case Seq((lastBoard, true)) => (lastBoard, number)
      case newBoards => playAll(newBoards.filterNot(_._2).map(_._1), remaining)
    }
  }

  private def calculateScore(board: Board, lastNumber: Int) = board.numbers.keys.sum * lastNumber

  private case class Board(numbers: Map[Int, Pos]) {
    def mark(number: Int) =
      numbers.get(number) match {
        case Some(pos) =>
          val newBoard = copy(numbers = numbers - number)
          (newBoard, newBoard.isBingo(pos))
        case None => (this, false)
      }

    private def isBingo(pos: Pos) = !numbers.values.exists(_.x == pos.x) || !numbers.values.exists(_.y == pos.y)
  }
}

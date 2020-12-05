package adventofcode.y2020

import scala.annotation.tailrec

object Day5 extends Year2020 {
  override val day = 5

  private val passes = input.getLines().map { line =>
    val binary = line.map {
      case 'F' | 'L' => '0'
      case 'B' | 'R' => '1'
    }
    BoardingPass(Integer.parseInt(binary.take(7), 2), Integer.parseInt(binary.drop(7), 2))
  }

  private val ids = passes.map(_.id).toSeq.sorted

  printDayPart(1, ids.last, "highest seat ID: %s")
  printDayPart(2, findFreeSeat(ids.head, ids.tail), "my seat ID: %s")

  @tailrec
  private def findFreeSeat(lastSeat: Int, remainingSeats: Seq[Int]): Int = {
    val current :: next = remainingSeats
    if (current - 1 == lastSeat)
      findFreeSeat(current, next)
    else
      current - 1
  }

  private case class BoardingPass(row: Int, column: Int) {
    def id: Int = row * 8 + column
  }
}

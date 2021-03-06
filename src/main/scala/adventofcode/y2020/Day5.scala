package adventofcode.y2020

import scala.annotation.tailrec

object Day5 extends Year2020 {
  override val day = 5

  private val seatIds = input.getLines().map { line =>
    val binary = line.map {
      case 'F' | 'L' => '0'
      case 'B' | 'R' => '1'
    }
    Integer.parseInt(binary, 2)
  }.toSeq.sorted

  printDayPart(1, seatIds.last, "highest seat ID: %s")
  printDayPart(2, findFreeSeat(seatIds.head, seatIds.tail), "my seat ID: %s")

  @tailrec
  private def findFreeSeat(lastSeat: Int, remainingSeats: Seq[Int]): Int = {
    val current :: next = remainingSeats
    if (current - 1 == lastSeat)
      findFreeSeat(current, next)
    else
      current - 1
  }
}

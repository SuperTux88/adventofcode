package adventofcode.y2020

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day5 extends Year2020 {
  override val day = 5

  override def runDay(input: BufferedSource): Unit = {
    val seatIds = input.getLines().map { line =>
      val binary = line.map {
        case 'F' | 'L' => '0'
        case 'B' | 'R' => '1'
      }
      Integer.parseInt(binary, 2)
    }.toList.sorted

    printDayPart(1, seatIds.last, "highest seat ID: %s")
    printDayPart(2, findFreeSeat(seatIds.head, seatIds.tail), "my seat ID: %s")
  }

  @tailrec
  private def findFreeSeat(lastSeat: Int, remainingSeats: List[Int]): Int = {
    val current :: next = remainingSeats: @unchecked
    if (current - 1 == lastSeat)
      findFreeSeat(current, next)
    else
      current - 1
  }
}

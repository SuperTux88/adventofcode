package adventofcode.y2022

import scala.annotation.tailrec
import scala.io.BufferedSource
import scala.math.pow

object Day25 extends Year2022 {
  override val day = 25

  private val SNAFU_VALUES = Map('2' -> 2, '1' -> 1, '0' -> 0, '-' -> -1, '=' -> -2)
  private val TO_SNAFU = SNAFU_VALUES.map(_.swap)

  override def runDay(input: BufferedSource): Unit = {
    val numbers = input.getLines().map(parseSnafu).toList
    printDayPart(1, toSnafu(numbers.sum), "Sum of SNAFU numbers: %s")
  }

  private def parseSnafu(snafu: String): Long =
    (1 to snafu.length).foldLeft(0L) { (acc, i) =>
      acc + SNAFU_VALUES(snafu(i - 1)) * pow(5, snafu.length - i).toLong
    }

  @tailrec
  private def toSnafu(number: Long, result: List[Char] = Nil): String =
    if (number == 0) result.mkString
    else {
      val (quotient, remainder) = (number / 5, number % 5)
      val offsetRemainder = if remainder > 2 then remainder - 5 else remainder
      val offsetQuotient = if remainder > 2 then quotient + 1 else quotient
      toSnafu(offsetQuotient, TO_SNAFU(offsetRemainder.toInt) :: result)
    }
}

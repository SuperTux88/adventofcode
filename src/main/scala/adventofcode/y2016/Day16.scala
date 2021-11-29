package adventofcode.y2016

import scala.annotation.tailrec

object Day16 extends Year2016 {
  override val day = 16

  override def runDay(seed: String): Unit = {
    printDayPart(1, calcChecksum(random(seed, 272)), "checksum for data: %s")
    printDayPart(2, calcChecksum(random(seed, 35651584)), "checksum for data: %s")
  }

  @tailrec
  private def random(seed: String, length: Int): String = {
    val data = dragonCurve(seed)

    if (data.length < length)
      random(data, length)
    else
      data.take(length)
  }

  private def dragonCurve(a: String) =
    s"${a}0${a.reverse.map(c => if (c == '0') '1' else '0')}"

  @tailrec
  private def calcChecksum(data: String): String = {
    val checksum = data.grouped(2).map {
      case "00"|"11" => '1'
      case _ => '0'
    }.mkString

    if (checksum.length % 2 == 0)
      calcChecksum(checksum)
    else
      checksum
  }
}

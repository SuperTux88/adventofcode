package adventofcode.y2016

import adventofcode.Logging
import adventofcode.common.OCR

import scala.io.BufferedSource

object Day8 extends Year2016 {
  override val day: Int = 8

  private val RectRE = """rect (\d+)x(\d+)""".r
  private val RotateRE = """rotate (column x|row y)=(\d+) by (\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val screen = Array.fill(6, 50)(false)

    input.getLines().foreach {
      case RectRE(width, height) =>
        for (x <- 0 until height.toInt; y <- 0 until width.toInt) screen(x)(y) = true
      case RotateRE(direction, index, pixels) => (direction, index.toInt, pixels.toInt) match {
        case ("column x", x, by) =>
          val column = rotate(screen.map(row => row(x)), by)
          screen.indices.foreach(row => screen(row)(x) = column(row))
        case ("row y", y, by) => screen(y) = rotate(screen(y), by)
        case command => throw new MatchError(s"Invalid instruction: $command")
      }
    }

    printDayPart(1, screen.map(_.count(identity)).sum, "total %s pixels are lit")

    val chars = (0 to 9).map(pos => getCharAt(screen, pos))

    printDayPart(2, chars.map(OCR.readChar).mkString, "the code is: %s")
    if (Logging.debug) screen.foreach(line => println(line.map(if (_) "█" else " ").mkString))
  }

  private def rotate(row: Array[Boolean], by: Int) = row.takeRight(by) ++ row.dropRight(by)

  private def getCharAt(screen: Array[Array[Boolean]], position: Int) = {
    val start = position * 5
    screen.map(line => line.slice(start, start + 5).map(if (_) 1 else 0).toSeq).toSeq
  }
}

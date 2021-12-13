package adventofcode.y2016

import adventofcode.Logging
import adventofcode.common.OCR
import adventofcode.common.pos.Pos

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

    val screenMap = OCR.convertToMap(screen.map(_.toSeq).toSeq, if _ then 1 else 0)
    if (Logging.debug) OCR.printImage(screenMap)

    printDayPart(2, OCR.readMessage(screenMap, 10, Pos(5, 6)), "the code is: %s")
  }

  private def rotate(row: Array[Boolean], by: Int) = row.takeRight(by) ++ row.dropRight(by)
}

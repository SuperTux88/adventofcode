package adventofcode.y2016

import adventofcode.Logging

object Day8 extends Year2016 {
  override val day: Int = 8

  val RectRE = """rect (\d+)x(\d+)""".r
  val RotateRE = """rotate (column x|row y)=(\d+) by (\d+)""".r

  val screen = Array.fill(6, 50)(false)

  input.getLines.foreach {
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

  private def rotate(row: Array[Boolean], by: Int) = row.takeRight(by) ++ row.dropRight(by)

  private def getCharAt(screen: Array[Array[Boolean]], position: Int) = {
    val start = position * 5
    screen.map(line => line.slice(start, start + 5).toList).toList
  }

  private object OCR {
    def readChar(char: List[List[Boolean]]): Char = chars.getOrElse(char, '_')

    // hardcoded chars for my solution
    private val chars = Map(
      'C' -> List(List(false, true, true, false, false), List(true, false, false, true, false), List(true, false, false, false, false), List(true, false, false, false, false), List(true, false, false, true, false), List(false, true, true, false, false)),
      'H' -> List(List(true, false, false, true, false), List(true, false, false, true, false), List(true, true, true, true, false), List(true, false, false, true, false), List(true, false, false, true, false), List(true, false, false, true, false)),
      'J' -> List(List(false, false, true, true, false), List(false, false, false, true, false), List(false, false, false, true, false), List(false, false, false, true, false), List(true, false, false, true, false), List(false, true, true, false, false)),
      'K' -> List(List(true, false, false, true, false), List(true, false, true, false, false), List(true, true, false, false, false), List(true, false, true, false, false), List(true, false, true, false, false), List(true, false, false, true, false)),
      'L' -> List(List(true, false, false, false, false), List(true, false, false, false, false), List(true, false, false, false, false), List(true, false, false, false, false), List(true, false, false, false, false), List(true, true, true, true, false)),
      'P' -> List(List(true, true, true, false, false), List(true, false, false, true, false), List(true, false, false, true, false), List(true, true, true, false, false), List(true, false, false, false, false), List(true, false, false, false, false)),
      'R' -> List(List(true, true, true, false, false), List(true, false, false, true, false), List(true, false, false, true, false), List(true, true, true, false, false), List(true, false, true, false, false), List(true, false, false, true, false)),
      'Y' -> List(List(true, false, false, false, true), List(true, false, false, false, true), List(false, true, false, true, false), List(false, false, true, false, false), List(false, false, true, false, false), List(false, false, true, false, false)),
      'Z' -> List(List(true, true, true, true, false), List(false, false, false, true, false), List(false, false, true, false, false), List(false, true, false, false, false), List(true, false, false, false, false), List(true, true, true, true, false))
    ).map(_.swap)
  }
}

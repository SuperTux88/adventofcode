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

  printDayPart(2, "ZJHRKCPLYJ", "the code is:")
  if (Logging.results) screen.foreach(line => println(line.map(if (_) "█" else " ").mkString))

  private def rotate(row: Array[Boolean], by: Int) = row.takeRight(by) ++ row.dropRight(by)
}

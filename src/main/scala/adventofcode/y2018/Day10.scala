package adventofcode.y2018

import adventofcode.Logging

import scala.annotation.tailrec

object Day10 extends Year2018 {
  override val day = 10

  private val PointRE = """position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>""".r

  private val points = input.getLines().map {
    case PointRE(x, y, xVelocity, yVelocity) => Point(x.toInt, y.toInt, xVelocity.toInt, yVelocity.toInt)
  }.toList

  private val (message, time) = searchMessage(points)

  if (Logging.debug) message.foreach(line => println(line.map(if (_) "█" else " ").mkString))

  private val chars = (0 until (message.head.length + 2) / 8).map(pos => getCharAt(message, pos))
  printDayPart(1, chars.map(OCR.readChar).mkString, "parsed message: %s")
  printDayPart(2, time)

  @tailrec
  private def searchMessage(points: List[Point], time: Int = 0): (Seq[Seq[Boolean]], Int) = {
    val (minX, maxX, minY, maxY) = (points.minBy(_.x).x, points.maxBy(_.x).x, points.minBy(_.y).y, points.maxBy(_.y).y)
    val diffY = maxY - minY

    if (diffY < 10) {
      val message = (minY to maxY).map(y => (minX to maxX).map(x => points.exists(p => p.x == x && p.y == y)))
      (message, time)
    } else {
      searchMessage(points.map(_.move()), time + 1)
    }
  }

  private def getCharAt(message: Seq[Seq[Boolean]], position: Int) = {
    val start = position * 8
    message.map(line => line.slice(start, start + 6).toList).toList
  }

  private case class Point(x: Int, y: Int, xVelocity: Int, yVelocity: Int) {
    def move(times: Int = 1) = Point(x + xVelocity * times, y + yVelocity * times, xVelocity, yVelocity)
  }

  private object OCR {
    def readChar(char: List[List[Boolean]]): Char = chars.getOrElse(char, '_')

    // hardcoded chars for as many solutions I could find :)
    private val chars = Map(
      'A' -> List(List(false, false, true, true, false, false), List(false, true, false, false, true, false), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, true, true, true, true, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true)),
      'B' -> List(List(true, true, true, true, true, false), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, true, true, true, true, false), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, true, true, true, true, false)),
      'C' -> List(List(false, true, true, true, true, false), List(true, false, false, false, false, true), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, true), List(false, true, true, true, true, false)),
      'E' -> List(List(true, true, true, true, true, true), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, true, true, true, true, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, true, true, true, true, true)),
      'F' -> List(List(true, true, true, true, true, true), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, true, true, true, true, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false)),
      'G' -> List(List(false, true, true, true, true, false), List(true, false, false, false, false, true), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, true, true, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, false, false, false, true, true), List(false, true, true, true, false, true)),
      'H' -> List(List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, true, true, true, true, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true)),
      'J' -> List(List(false, false, false, true, true, true), List(false, false, false, false, true, false), List(false, false, false, false, true, false), List(false, false, false, false, true, false), List(false, false, false, false, true, false), List(false, false, false, false, true, false), List(false, false, false, false, true, false), List(true, false, false, false, true, false), List(true, false, false, false, true, false), List(false, true, true, true, false, false)),
      'K' -> List(List(true, false, false, false, false, true), List(true, false, false, false, true, false), List(true, false, false, true, false, false), List(true, false, true, false, false, false), List(true, true, false, false, false, false), List(true, true, false, false, false, false), List(true, false, true, false, false, false), List(true, false, false, true, false, false), List(true, false, false, false, true, false), List(true, false, false, false, false, true)),
      'L' -> List(List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, true, true, true, true, true)),
      'N' -> List(List(true, false, false, false, false, true), List(true, true, false, false, false, true), List(true, true, false, false, false, true), List(true, false, true, false, false, true), List(true, false, true, false, false, true), List(true, false, false, true, false, true), List(true, false, false, true, false, true), List(true, false, false, false, true, true), List(true, false, false, false, true, true), List(true, false, false, false, false, true)),
      'P' -> List(List(true, true, true, true, true, false), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, true, true, true, true, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false)),
      'R' -> List(List(true, true, true, true, true, false), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(true, true, true, true, true, false), List(true, false, false, true, false, false), List(true, false, false, false, true, false), List(true, false, false, false, true, false), List(true, false, false, false, false, true), List(true, false, false, false, false, true)),
      'X' -> List(List(true, false, false, false, false, true), List(true, false, false, false, false, true), List(false, true, false, false, true, false), List(false, true, false, false, true, false), List(false, false, true, true, false, false), List(false, false, true, true, false, false), List(false, true, false, false, true, false), List(false, true, false, false, true, false), List(true, false, false, false, false, true), List(true, false, false, false, false, true)),
      'Z' -> List(List(true, true, true, true, true, true), List(false, false, false, false, false, true), List(false, false, false, false, false, true), List(false, false, false, false, true, false), List(false, false, false, true, false, false), List(false, false, true, false, false, false), List(false, true, false, false, false, false), List(true, false, false, false, false, false), List(true, false, false, false, false, false), List(true, true, true, true, true, true)),
    ).map(_.swap)
  }
}

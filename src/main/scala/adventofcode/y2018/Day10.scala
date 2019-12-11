package adventofcode.y2018

import adventofcode.Logging
import adventofcode.common.OCR

object Day10 extends Year2018 {
  override val day = 10

  private val PointRE = """position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>""".r

  private val points = input.getLines().map {
    case PointRE(x, y, xVelocity, yVelocity) => Point(x.toInt, y.toInt, xVelocity.toInt, yVelocity.toInt)
  }.toList

  private val (minY, maxY) = (points.minBy(_.y).y, points.maxBy(_.y).y)
  private val (minVelY, maxVelY) = (points.minBy(_.yVelocity).yVelocity, points.maxBy(_.yVelocity).yVelocity)
  private val timeNeeded = (maxY - minY) / (minVelY - maxVelY).abs

  private val message = getMessage(points.map(_.move(timeNeeded)))

  if (Logging.debug) message.foreach(line => println(line.map(if (_) "█" else " ").mkString))

  private val chars = (0 until (message.head.length + 2) / 8).map(pos => getCharAt(message, pos))
  printDayPart(1, chars.map(OCR.readChar).mkString, "parsed message: %s")
  printDayPart(2, timeNeeded)

  private def getMessage(points: List[Point]) = {
    val (minX, maxX, minY, maxY) = (points.minBy(_.x).x, points.maxBy(_.x).x, points.minBy(_.y).y, points.maxBy(_.y).y)
    (minY to maxY).map(y => (minX to maxX).map(x => points.exists(p => p.x == x && p.y == y)).toList).toList
  }

  private def getCharAt(message: Seq[Seq[Boolean]], position: Int) = {
    val start = position * 8
    message.map(line => line.slice(start, start + 6).map(if (_) 1 else 0))
  }

  private case class Point(x: Int, y: Int, xVelocity: Int, yVelocity: Int) {
    def move(times: Int = 1) = Point(x + xVelocity * times, y + yVelocity * times, xVelocity, yVelocity)
  }
}

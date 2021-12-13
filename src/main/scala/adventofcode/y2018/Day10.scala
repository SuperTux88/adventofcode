package adventofcode.y2018

import adventofcode.Logging
import adventofcode.common.OCR
import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day10 extends Year2018 {
  override val day = 10

  private val PointRE = """position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>""".r

  override def runDay(input: BufferedSource): Unit = {
    val points = input.getLines().map {
      case PointRE(x, y, xVelocity, yVelocity) => Point(x.toInt, y.toInt, xVelocity.toInt, yVelocity.toInt)
    }.toList

    val (minY, maxY) = (points.minBy(_.y).y, points.maxBy(_.y).y)
    val (minVelY, maxVelY) = (points.minBy(_.yVelocity).yVelocity, points.maxBy(_.yVelocity).yVelocity)
    val timeNeeded = (maxY - minY) / (minVelY - maxVelY).abs

    val message = getMessage(points.map(_.move(timeNeeded)))
    val messageMap = OCR.convertToMap(message, if _ then 1 else 0)
    if (Logging.debug) OCR.printImage(messageMap)

    val numberOfChars = (message.head.length + 2) / 8
    printDayPart(1, OCR.readMessage(messageMap, numberOfChars, Pos(6, 10), space = 2), "parsed message: %s")
    printDayPart(2, timeNeeded)
  }

  private def getMessage(points: List[Point]) = {
    val (minX, maxX, minY, maxY) = (points.minBy(_.x).x, points.maxBy(_.x).x, points.minBy(_.y).y, points.maxBy(_.y).y)
    (minY to maxY).map(y => (minX to maxX).map(x => points.exists(p => p.x == x && p.y == y)).toList).toList
  }

  private case class Point(x: Int, y: Int, xVelocity: Int, yVelocity: Int) {
    def move(times: Int = 1): Point = Point(x + xVelocity * times, y + yVelocity * times, xVelocity, yVelocity)
  }
}

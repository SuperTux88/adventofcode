package adventofcode.y2021

import adventofcode.Logging
import adventofcode.common.OCR
import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day13 extends Year2021 {
  override val day = 13

  private val PointRE = """(\d+),(\d+)""".r
  private val FoldRE = """fold along (\w)=(\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()
    val points = lines.takeWhile(_.nonEmpty).map {
      case PointRE(x, y) => Pos(x.toInt, y.toInt)
    }.toSet
    val folds = lines.takeWhile(_.nonEmpty).map {
      case FoldRE("x", x) => FoldX(x.toInt)
      case FoldRE("y", y) => FoldY(y.toInt)
    }

    val afterFirstfold = fold(points, folds.next)
    printDayPart(1, afterFirstfold.size, "points after first fold: %s")

    val finishedPoints = folds.foldLeft(afterFirstfold)(fold)
    val pointsMap = finishedPoints.map(_ -> 1).toMap.withDefaultValue(0)

    if (Logging.debug) Pos.printMap(pointsMap, _ match {
      case 0 => ' '
      case 1 => '█'
    })
    val chars = (0 until 8).map(getCharAt(pointsMap, _))
    printDayPart(2, chars.map(OCR.readChar).mkString, "code to activate the infrared thermal imaging camera system: %s")
  }

  private def fold(points: Set[Pos], fold: Fold): Set[Pos] =
    fold match {
      case FoldX(x) =>
        points.map(point => if (point.x > x) then point.copy(x = -(point.x - x) + x) else point)
      case FoldY(y) =>
        points.map(point => if (point.y > y) then point.copy(y = -(point.y - y) + y) else point)
    }

  private def getCharAt(image: Map[Pos, Int], position: Int) = {
    val start = position * 5
    (0 until 6).map(y => (start until start + 5).map(x => image(Pos(x, y))))
  }

  private sealed trait Fold
  private case class FoldX(x: Int) extends Fold
  private case class FoldY(y: Int) extends Fold
}

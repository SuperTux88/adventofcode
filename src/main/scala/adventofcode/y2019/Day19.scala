package adventofcode.y2019

import adventofcode.common.pos.Pos

import scala.annotation.tailrec

object Day19 extends Year2019 {
  override val day = 19

  override def runDay(intCode: IntCode): Unit = {
    val scanned50x50 = (0 until 50).map(y => (0 until 50).map(x => isInBeam(intCode, Pos(x, y))))
    printDayPart(1, scanned50x50.flatten.count(identity), "number of points in beam: %s")

    val topLeft = findSquare(intCode, Pos(50, scanned50x50.zipWithIndex.findLast(_._1.last).map(_._2).getOrElse(50)))
    printDayPart(2, topLeft.x * 10000 + topLeft.y)
  }

  private def isInBeam(intCode: IntCode, pos: Pos) = intCode.run(Vector(pos.x.toLong, pos.y.toLong)).output.next() == 1

  @tailrec
  private def findSquare(intCode: IntCode, pos: Pos, size: Int = 100): Pos =
    if (isInBeam(intCode, pos + (size - 1, 0)))
      if (isInBeam(intCode, pos + (0, size - 1)))
        pos
      else
        findSquare(intCode, pos.right, size)
    else
      findSquare(intCode, pos.down, size)
}

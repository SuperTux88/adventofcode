package adventofcode.y2019

import adventofcode.common.Pos

import scala.annotation.tailrec

object Day19 extends Year2019 {
  override val day = 19

  private val intCode = new IntCode(inputString)

  private val scanned50x50 = (0 until 50).map(y => (0 until 50).map(x => isInBeam(Pos(x, y))))
  printDayPart(1, scanned50x50.flatten.count(identity), "number of points in beam: %s")

  private val topLeft = findSquare(Pos(50, scanned50x50.zipWithIndex.findLast(_._1.last).map(_._2).getOrElse(50)))
  printDayPart(2, topLeft.x * 10000 + topLeft.y)

  private def isInBeam(pos: Pos) = intCode.run(Vector(pos.x.toLong, pos.y.toLong)).output.next == 1

  @tailrec
  private def findSquare(pos: Pos, size: Int = 100): Pos =
    if (isInBeam(pos + (size - 1, 0)))
      if (isInBeam(pos + (0, size - 1)))
        pos
      else
        findSquare(pos.right, size)
    else
      findSquare(pos.down, size)
}

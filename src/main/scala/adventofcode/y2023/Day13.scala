package adventofcode.y2023

import adventofcode.common.pos.Pos

object Day13 extends Year2023 {
  override val day = 13

  override def runDay(input: String): Unit = {
    val patterns = input.split("\n\n").map(pattern => {
      Pos.parseMap(pattern.split("\n").iterator, _ == '#')
    }).toList

    printDayPart(1, patterns.map(findMirror(_)).sum, "Summary of pattern notes: %s")
    printDayPart(2, patterns.map(findMirror(_, 1)).sum, "Summary of pattern notes with imperfection: %s")
  }

  private def findMirror(pattern: Map[Pos, Boolean], imperfection: Int = 0): Int = {
    val size = pattern.keys.max

    def mirrorXPerfection(mirror: Int) = mirrorRange(mirror, size.x).map(x =>
      val mirrorX = mirrorValue(mirror, x)
      size.y - (0 to size.y).count(y => pattern(Pos(x, y)) == pattern(Pos(mirrorX, y))) + 1
    ).sum

    def mirrorYPerfection(mirror: Int) = mirrorRange(mirror, size.y).map(y =>
      val mirrorY = mirrorValue(mirror, y)
      size.x - (0 to size.x).count(x => pattern(Pos(x, y)) == pattern(Pos(x, mirrorY))) + 1
    ).sum

    (0 until size.x).find(mirror => mirrorXPerfection(mirror) == imperfection)
      .map(_ + 1)
      .orElse((0 until size.y).find(y => mirrorYPerfection(y) == imperfection)
        .map(y => (y + 1) * 100))
      .getOrElse(0)
  }

  private def mirrorRange(mirror: Int, size: Int) = if (mirror >= size / 2) mirror + 1 to size else 0 to mirror
  private def mirrorValue(mirror: Int, value: Int): Int = mirror * 2 - value + 1
}

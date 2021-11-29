package adventofcode.y2015

import scala.io.BufferedSource

object Day2 extends Year2015 {
  override val day: Int = 2

  override def runDay(input: BufferedSource): Unit = {
    var totalPaper = 0
    var totalRibbon = 0

    input.getLines().foreach { present =>
      val Array(l, w, h) = present.split("x").map(_.toInt).sorted

      totalPaper += (2 * l * w + 2 * w * h + 2 * h * l) + l * w
      totalRibbon += (l + l + w + w) + (l * w * h)
    }

    printDayPart(1, totalPaper, "total square feet of wrapping paper: %s")
    printDayPart(2, totalRibbon, "total feet of ribbon: %s")
  }
}

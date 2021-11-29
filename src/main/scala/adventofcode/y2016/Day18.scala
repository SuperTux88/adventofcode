package adventofcode.y2016

import scala.io.BufferedSource

object Day18 extends Year2016 {
  override val day = 18

  override def runDay(input: BufferedSource): Unit = {
    val firstRow = input.map(_ == '.').toList

    printDayPart(1, countSafe(firstRow, 40))
    printDayPart(2, countSafe(firstRow, 400000))
  }

  private def countSafe(firstRow: List[Boolean], rows: Int) = {
    val lastIndex = firstRow.length - 1
    val rowRange = 0 to lastIndex

    (1 until rows).foldLeft((firstRow.count(identity), firstRow)) { (state, _) =>
      val (count, lastRow) = state

      val row = rowRange.map {
        case 0 => lastRow(1)
        case i if i == lastIndex => lastRow(i - 1)
        case i => lastRow(i - 1) == lastRow(i + 1)
      }.toList

      (count + row.count(identity), row)
    }._1
  }
}

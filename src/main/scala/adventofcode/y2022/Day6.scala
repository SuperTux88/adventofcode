package adventofcode.y2022

object Day6 extends Year2022 {
  override val day = 6

  override def runDay(input: String): Unit = {
    printDayPart(1, getFirstStartOfPackatIndex(input, 4),
      "First start-of-packet marker after %s characters")
    printDayPart(2, getFirstStartOfPackatIndex(input, 14),
      "First start-of-packet marker with 14 characters after %s characters")
  }

  private def getFirstStartOfPackatIndex(datastream: String, markerSize: Int) =
    datastream.sliding(markerSize).takeWhile(_.toSet.size != markerSize).length + markerSize
}

package adventofcode.y2016

object Day9 extends Year2016 {
  override val day: Int = 9

  private val MarkerRE = """([^(]*)\((\d+)x(\d+)\)(.*)""".r

  override def runDay(compressed: String): Unit = {
    printDayPart(1, decompressedLength(compressed), "length after simple decompress: %s")
    printDayPart(2, decompressedLength(compressed, recursive = true), "length after recursive decompress: %s")
  }

  private def decompressedLength(string: String, recursive: Boolean = false): Long = string match {
    case MarkerRE(before, count, multiplier, after) =>
      val (selected, remainder) = after.splitAt(count.toInt)

      val length = if (recursive) decompressedLength(selected, recursive) else selected.length

      before.length + length * multiplier.toInt + decompressedLength(remainder, recursive)
    case _ => string.length
  }
}

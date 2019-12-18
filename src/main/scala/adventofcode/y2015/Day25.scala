package adventofcode.y2015

object Day25 extends Year2015 {
  override val day: Int = 25

  val InputRE = """Enter the code at row (\d+), column (\d+).""".r.unanchored

  val (row, col) = inputString match {
    case InputRE(rowStr, colStr) => (rowStr.toInt, colStr.toInt)
  }

  val startCode = 20151125 * BigInt(252533).modPow(indexOf(row, col), 33554393) % 33554393

  printDayPart(1, startCode.toInt, "the start code is: %s")

  private def indexOf(row: Int, col: Int) = {
    val maxRow = row + col - 1
    val topRight = maxRow * (maxRow+1) / 2
    topRight - row
  }
}

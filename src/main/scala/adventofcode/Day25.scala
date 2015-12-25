package adventofcode

object Day25 extends DayApp {
  override val day: Int = 25

  val InputRE = """Enter the code at row (\d+), column (\d+).""".r.unanchored

  val (row, col) = input.mkString match {
    case InputRE(rowStr, colStr) => (rowStr.toInt, colStr.toInt)
  }

  val startCode = (2 to indexOf(row, col)).foldLeft(BigInt(20151125))((code, index) => code * 252533 % 33554393)

  printDayPart(1, startCode.toInt)

  def indexOf(row: Int, col: Int) = {
    val maxRow = row + col - 1
    val topRight = maxRow * (maxRow+1) / 2
    topRight - row + 1
  }
}

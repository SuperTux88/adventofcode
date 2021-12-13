package adventofcode.common

import adventofcode.common.pos.Pos

object OCR {
  def convertToMap[A](image: Seq[Seq[A]], convert: A => Int): Map[Pos, Int] = image.zipWithIndex.flatMap {
    case (line, y) => line.zipWithIndex.map {
      case (value, x) => Pos(x, y) -> convert(value)
    }
  }.toMap

  def printImage(image: Map[Pos, Int]) = Pos.printMap(image, _ match {
    case 0 => ' '
    case 1 => '█'
  })

  def readMessage(image: Map[Pos, Int], numberOfChars: Int, charSize: Pos, offset: Pos = Pos(0, 0), space: Int = 0) =
    (0 until numberOfChars).map(pos => readChar(getCharAt(image, pos, charSize, offset, space))).mkString

  def getCharAt(image: Map[Pos, Int], position: Int, charSize: Pos, offset: Pos = Pos(0, 0), space: Int = 0) = {
    val start = position * (charSize.x + space) + offset.x
    (offset.y until charSize.y + offset.y).map(y => (start until start + charSize.x).map(x => image(Pos(x, y))))
  }

  def readChar(char: Seq[Seq[Int]]): Char =
    fonts(char.head.length, char.length).getOrElse(char, '_')

  // hardcoded chars for as many solutions I could find :)
  private val fonts =
    Map(
      (5, 6) ->
        Map(
          'A' -> Seq(Seq(0, 1, 1, 0, 0), Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0), Seq(1, 1, 1, 1, 0), Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0)),
          'B' -> Seq(Seq(1, 1, 1, 0, 0), Seq(1, 0, 0, 1, 0), Seq(1, 1, 1, 0, 0), Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0), Seq(1, 1, 1, 0, 0)),
          'C' -> Seq(Seq(0, 1, 1, 0, 0), Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0), Seq(1, 0, 0, 1, 0), Seq(0, 1, 1, 0, 0)),
          'E' -> Seq(Seq(1, 1, 1, 1, 0), Seq(1, 0, 0, 0, 0), Seq(1, 1, 1, 0, 0), Seq(1, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0), Seq(1, 1, 1, 1, 0)),
          'F' -> Seq(Seq(1, 1, 1, 1, 0), Seq(1, 0, 0, 0, 0), Seq(1, 1, 1, 0, 0), Seq(1, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0)),
          'G' -> Seq(Seq(0, 1, 1, 0, 0), Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 0, 0), Seq(1, 0, 1, 1, 0), Seq(1, 0, 0, 1, 0), Seq(0, 1, 1, 1, 0)),
          'H' -> Seq(Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0), Seq(1, 1, 1, 1, 0), Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0)),
          'J' -> Seq(Seq(0, 0, 1, 1, 0), Seq(0, 0, 0, 1, 0), Seq(0, 0, 0, 1, 0), Seq(0, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0), Seq(0, 1, 1, 0, 0)),
          'K' -> Seq(Seq(1, 0, 0, 1, 0), Seq(1, 0, 1, 0, 0), Seq(1, 1, 0, 0, 0), Seq(1, 0, 1, 0, 0), Seq(1, 0, 1, 0, 0), Seq(1, 0, 0, 1, 0)),
          'L' -> Seq(Seq(1, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0), Seq(1, 1, 1, 1, 0)),
          'O' -> Seq(Seq(0, 1, 1, 0, 0), Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0), Seq(0, 1, 1, 0, 0)),
          'P' -> Seq(Seq(1, 1, 1, 0, 0), Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0), Seq(1, 1, 1, 0, 0), Seq(1, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0)),
          'R' -> Seq(Seq(1, 1, 1, 0, 0), Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0), Seq(1, 1, 1, 0, 0), Seq(1, 0, 1, 0, 0), Seq(1, 0, 0, 1, 0)),
          'S' -> Seq(Seq(0, 1, 1, 1, 0), Seq(1, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0), Seq(0, 1, 1, 0, 0), Seq(0, 0, 0, 1, 0), Seq(1, 1, 1, 0, 0)),
          'U' -> Seq(Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0), Seq(0, 1, 1, 0, 0)),
          'Y' -> Seq(Seq(1, 0, 0, 0, 1), Seq(1, 0, 0, 0, 1), Seq(0, 1, 0, 1, 0), Seq(0, 0, 1, 0, 0), Seq(0, 0, 1, 0, 0), Seq(0, 0, 1, 0, 0)),
          'Z' -> Seq(Seq(1, 1, 1, 1, 0), Seq(0, 0, 0, 1, 0), Seq(0, 0, 1, 0, 0), Seq(0, 1, 0, 0, 0), Seq(1, 0, 0, 0, 0), Seq(1, 1, 1, 1, 0)),
        ).map(_.swap),
      (6, 10) ->
        Map(
          'A' -> Seq(Seq(0, 0, 1, 1, 0, 0), Seq(0, 1, 0, 0, 1, 0), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 1, 1, 1, 1, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1)),
          'B' -> Seq(Seq(1, 1, 1, 1, 1, 0), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 1, 1, 1, 1, 0), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 1, 1, 1, 1, 0)),
          'C' -> Seq(Seq(0, 1, 1, 1, 1, 0), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 1), Seq(0, 1, 1, 1, 1, 0)),
          'E' -> Seq(Seq(1, 1, 1, 1, 1, 1), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 1, 1, 1, 1, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 1, 1, 1, 1, 1)),
          'F' -> Seq(Seq(1, 1, 1, 1, 1, 1), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 1, 1, 1, 1, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0)),
          'G' -> Seq(Seq(0, 1, 1, 1, 1, 0), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 1, 1, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 1, 1), Seq(0, 1, 1, 1, 0, 1)),
          'H' -> Seq(Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 1, 1, 1, 1, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1)),
          'J' -> Seq(Seq(0, 0, 0, 1, 1, 1), Seq(0, 0, 0, 0, 1, 0), Seq(0, 0, 0, 0, 1, 0), Seq(0, 0, 0, 0, 1, 0), Seq(0, 0, 0, 0, 1, 0), Seq(0, 0, 0, 0, 1, 0), Seq(0, 0, 0, 0, 1, 0), Seq(1, 0, 0, 0, 1, 0), Seq(1, 0, 0, 0, 1, 0), Seq(0, 1, 1, 1, 0, 0)),
          'K' -> Seq(Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 1, 0), Seq(1, 0, 0, 1, 0, 0), Seq(1, 0, 1, 0, 0, 0), Seq(1, 1, 0, 0, 0, 0), Seq(1, 1, 0, 0, 0, 0), Seq(1, 0, 1, 0, 0, 0), Seq(1, 0, 0, 1, 0, 0), Seq(1, 0, 0, 0, 1, 0), Seq(1, 0, 0, 0, 0, 1)),
          'L' -> Seq(Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 1, 1, 1, 1, 1)),
          'N' -> Seq(Seq(1, 0, 0, 0, 0, 1), Seq(1, 1, 0, 0, 0, 1), Seq(1, 1, 0, 0, 0, 1), Seq(1, 0, 1, 0, 0, 1), Seq(1, 0, 1, 0, 0, 1), Seq(1, 0, 0, 1, 0, 1), Seq(1, 0, 0, 1, 0, 1), Seq(1, 0, 0, 0, 1, 1), Seq(1, 0, 0, 0, 1, 1), Seq(1, 0, 0, 0, 0, 1)),
          'P' -> Seq(Seq(1, 1, 1, 1, 1, 0), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 1, 1, 1, 1, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0)),
          'R' -> Seq(Seq(1, 1, 1, 1, 1, 0), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(1, 1, 1, 1, 1, 0), Seq(1, 0, 0, 1, 0, 0), Seq(1, 0, 0, 0, 1, 0), Seq(1, 0, 0, 0, 1, 0), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1)),
          'X' -> Seq(Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1), Seq(0, 1, 0, 0, 1, 0), Seq(0, 1, 0, 0, 1, 0), Seq(0, 0, 1, 1, 0, 0), Seq(0, 0, 1, 1, 0, 0), Seq(0, 1, 0, 0, 1, 0), Seq(0, 1, 0, 0, 1, 0), Seq(1, 0, 0, 0, 0, 1), Seq(1, 0, 0, 0, 0, 1)),
          'Z' -> Seq(Seq(1, 1, 1, 1, 1, 1), Seq(0, 0, 0, 0, 0, 1), Seq(0, 0, 0, 0, 0, 1), Seq(0, 0, 0, 0, 1, 0), Seq(0, 0, 0, 1, 0, 0), Seq(0, 0, 1, 0, 0, 0), Seq(0, 1, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 0, 0, 0, 0, 0), Seq(1, 1, 1, 1, 1, 1)),
        ).map(_.swap),
    )
}

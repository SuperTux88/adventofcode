package adventofcode.y2019

import adventofcode.Logging

object Day8 extends Year2019 {
  override val day = 8

  private val width = 25
  private val height = 6
  private val layerSize = width * height

  private val encodedImage = input.map(_.asDigit).sliding(layerSize, layerSize).toSeq

  private val fewestZeros = encodedImage.minBy(_.count(_ == 0))
  printDayPart(1, fewestZeros.count(_ == 1) * fewestZeros.count(_ == 2))

  private val decodedImage = encodedImage.transpose.map(_.find(_ != 2).get).sliding(width, width).toSeq

  if (Logging.debug) {
    decodedImage.foreach { line =>
      println(line.map {
        case 0 => " "
        case 1 => "█"
      }.mkString)
    }
  }

  private val chars = (0 until width / 5).map(pos => getCharAt(decodedImage, pos))
  printDayPart(2, chars.map(OCR.readChar).mkString, "parsed password: %s")

  private def getCharAt(image: Seq[Seq[Int]], position: Int) = {
    val start = position * 5
    image.map(line => line.slice(start, start + 4))
  }

  private object OCR {
    def readChar(char: Seq[Seq[Int]]): Char = chars.getOrElse(char, '_')

    // hardcoded chars for as many solutions I could find :)
    private val chars = Map(
      'C' -> Seq(Seq(0, 1, 1, 0), Seq(1, 0, 0, 1), Seq(1, 0, 0, 0), Seq(1, 0, 0, 0), Seq(1, 0, 0, 1), Seq(0, 1, 1, 0)),
      'G' -> Seq(Seq(0, 1, 1, 0), Seq(1, 0, 0, 1), Seq(1, 0, 0, 0), Seq(1, 0, 1, 1), Seq(1, 0, 0, 1), Seq(0, 1, 1, 1)),
      'H' -> Seq(Seq(1, 0, 0, 1), Seq(1, 0, 0, 1), Seq(1, 1, 1, 1), Seq(1, 0, 0, 1), Seq(1, 0, 0, 1), Seq(1, 0, 0, 1)),
      'L' -> Seq(Seq(1, 0, 0, 0), Seq(1, 0, 0, 0), Seq(1, 0, 0, 0), Seq(1, 0, 0, 0), Seq(1, 0, 0, 0), Seq(1, 1, 1, 1)),
      'P' -> Seq(Seq(1, 1, 1, 0), Seq(1, 0, 0, 1), Seq(1, 0, 0, 1), Seq(1, 1, 1, 0), Seq(1, 0, 0, 0), Seq(1, 0, 0, 0)),
    ).map(_.swap)
  }
}

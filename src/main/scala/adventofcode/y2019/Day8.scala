package adventofcode.y2019

import adventofcode.Logging
import adventofcode.common.OCR

import scala.io.BufferedSource

object Day8 extends Year2019 {
  override val day = 8

  private val width = 25
  private val height = 6
  private val layerSize = width * height

  override def runDay(input: BufferedSource): Unit = {
    val encodedImage = input.map(_.asDigit).takeWhile(_ >= 0).grouped(layerSize).toSeq

    val fewestZeros = encodedImage.minBy(_.count(_ == 0))
    printDayPart(1, fewestZeros.count(_ == 1) * fewestZeros.count(_ == 2))

    val decodedImage = encodedImage.transpose.map(_.find(_ != 2).get).grouped(width).toSeq

    if (Logging.debug) {
      decodedImage.foreach { line =>
        println(line.map {
          case 0 => " "
          case 1 => "█"
        }.mkString)
      }
    }

    val chars = (0 until 5).map(pos => getCharAt(decodedImage, pos))
    printDayPart(2, chars.map(OCR.readChar).mkString, "parsed password: %s")
  }

  private def getCharAt(image: Seq[Seq[Int]], position: Int) = {
    val start = position * 5
    image.map(line => line.slice(start, start + 5))
  }
}

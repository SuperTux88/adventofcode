package adventofcode.y2019

import adventofcode.Logging
import adventofcode.common.OCR
import adventofcode.common.pos.Pos

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
    val decodedImageMap = OCR.convertToMap(decodedImage, identity)
    if (Logging.debug) OCR.printImage(decodedImageMap)

    printDayPart(2, OCR.readMessage(decodedImageMap, 5, Pos(5, 6)), "parsed password: %s")
  }
}

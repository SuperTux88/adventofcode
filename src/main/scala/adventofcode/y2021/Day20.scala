package adventofcode.y2021

import adventofcode.common.pos.Pos

import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day20 extends Year2021 {
  override val day = 20

  private val pixelGroupOffsets = (for (y <- -1 to 1; x <- -1 to 1) yield (x, y)).toList
  private val index0 = List.fill(9)(false)
  private val index511 = List.fill(9)(true)

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()
    val algorithm = lines.next.map(_ == '#').zipWithIndex.map { case (value, index) =>
      f"${index.toBinaryString.toInt}%09d".map(_ == '1').toList -> value
    }.toMap
    val image = lines.drop(1).takeWhile(_.nonEmpty).map(_.map(_ == '#').toVector).toVector

    val result2 = run(image, 2, algorithm)
    printDayPart(1, count(result2), "lit pixels after 2 iterations: %s")

    val result50 = run(image, 50, algorithm)
    printDayPart(2, count(result50), "lit pixels after 50 iterations: %s")
  }

  private def run(image: Vector[Vector[Boolean]], cyclesToRun: Int, algorithm: Map[List[Boolean], Boolean]) =
    (1 to cyclesToRun).foldLeft(image, false) { case ((currentImage, infinity), index) =>
      val (width, height) = (currentImage(0).size, currentImage.size)
      val newImage = (-1 to width + 1).par.map { y =>
        (-1 to height + 1).map { x =>
          algorithm(getPixelValue(Pos(x, y), currentImage, Pos(width, height), infinity))
        }.toVector
      }.toVector
      (newImage, algorithm(if infinity then index511 else index0))
    }._1

  private def getPixelValue(pos: Pos, image: Vector[Vector[Boolean]], size: Pos, default: Boolean) =
    pixelGroupOffsets.map(offset => pos + offset).map { (pos) =>
      if pos.x >= 0 && pos.x < size.x && pos.y >= 0 && pos.y < size.y then image(pos.y)(pos.x) else default
    }

  private def count(image: Vector[Vector[Boolean]]) = image.map(_.count(identity)).sum
}

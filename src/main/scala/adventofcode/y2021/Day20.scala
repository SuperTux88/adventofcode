package adventofcode.y2021

import adventofcode.common.misc.ConwaysGameOfLife
import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day20 extends Year2021 {
  override val day = 20

  private val pixelGroupOffsets = (for (y <- -1 to 1; x <- -1 to 1) yield (x, y)).toList

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()
    val algorithm = lines.next.map(_ == '#').zipWithIndex.map { case (value, index) =>
      f"${index.toBinaryString.toInt}%09d".map(_ == '1').toList -> value
    }.toMap
    val image = lines.drop(1).takeWhile(_.nonEmpty).zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.flatMap {
        case ('#', x) => Some(Pos(x, y))
        case _ => None
      }
    }.toSet

    val result2 = run(image, 2, algorithm)
    printDayPart(1, result2.size, "lit pixels after 2 iterations: %s")

    val result50 = run(image, 50, algorithm)
    printDayPart(2, result50.size, "lit pixels after 50 iterations: %s")
  }

  private def run(image: Set[Pos], cyclesToRun: Int, algorithm: Map[List[Boolean], Boolean]): Set[Pos] =
    ConwaysGameOfLife.run(image, cyclesToRun, nextPixelState(algorithm), nextSize(image.min, image.max), getPixelKey)

  private def nextPixelState(algorithm: Map[List[Boolean], Boolean])(_active: Boolean, algorithmKey: List[Boolean]) =
    algorithm(algorithmKey)

  private def nextSize(originalSize: (Pos, Pos))(_image: Set[Pos], cycle: Int): Set[Pos] = {
    val extra = cycle % 2 match {
      case 0 => cycle * 3
      case 1 => cycle * 3 + 4
    }
    val (min, max) = originalSize
    (for (x <- min.x - extra to max.x + extra; y <- min.y - extra to max.y + extra) yield Pos(x, y)).toSet
  }

  private def getPixelKey(pos: Pos, image: Set[Pos]): List[Boolean] =
    pixelGroupOffsets.map(offset => image.contains(pos + offset))
}

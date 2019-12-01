package adventofcode.y2018

import scala.collection.parallel.CollectionConverters._

object Day11 extends Year2018 {
  override val day = 11

  val serialNumber = input.mkString.toInt
  val grid = (11 to 310).map(rackID => (1 to 300).map(y => ((rackID * y + serialNumber) * rackID /100) % 10 - 5))

  val partialSums = (for (x <- 1 to 300; y <- 1 to 300) yield (x,y))
    .foldLeft(Map[(Int, Int), Int]().withDefaultValue(0)) { (sums, pos) =>
      val (x, y) = pos
      sums + (pos -> (sums(x, y-1) + sums(x-1, y) - sums(x-1, y-1) + grid(x-1)(y-1)))
    }

  def sumSquare(x: Int, y: Int, size: Int = 3) =
    partialSums(x+size-1, y+size-1) - partialSums(x-1, y+size-1) - partialSums(x+size-1, y-1) + partialSums(x-1, y-1)

  val largestBySize = (1 to 300).par.map { size =>
    val powerSums = for (x <- 1 to 301 - size; y <- 1 to 301 - size)
      yield (x, y, size, sumSquare(x, y, size))
    size -> powerSums.maxBy(_._4)
  }.toMap

  val (x3, y3, _, _) = largestBySize(3)
  printDayPart(1, s"$x3,$y3", "best 3x3: %s")

  val (x, y, size, _) = largestBySize.maxBy(_._2._4)._2
  printDayPart(2, s"$x,$y,$size", "best x,y,size: %s")
}

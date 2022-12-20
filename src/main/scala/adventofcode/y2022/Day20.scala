package adventofcode.y2022

import scala.io.BufferedSource
import adventofcode.common.NumberHelper.ExtendedIntegral

object Day20 extends Year2022 {
  override val day = 20

  override def runDay(input: BufferedSource): Unit = {
    val numbers = input.getLines().zipWithIndex.map((l, i) => Number(l.toLong, i)).toVector

    val shuffled = numbers.foldLeft(numbers)(moveNumber)

    printDayPart(1, getGroveCoordinates(shuffled).sum, "Sum of grove coordinates: %s")

    val decrypted = numbers.map(n => n.copy(value = n.value * 811589153L))
    val shuffled10 = (1 to 10).foldLeft(decrypted) { (nums, _) =>
      decrypted.foldLeft(nums)(moveNumber)
    }

    printDayPart(2, getGroveCoordinates(shuffled10).sum, "Sum of grove coordinates with decription key: %s")
  }

  private def moveNumber(numbers: Vector[Number], number: Number): Vector[Number] = {
    val without = numbers.filterNot(_ == number)
    val (before, after) = without.splitAt(((numbers.indexOf(number) + number.value) %+ without.size).toInt)
    before ++ Vector(number) ++ after
  }

  private def getGroveCoordinates(numbers: Vector[Number]): List[Long] = {
    val zero = numbers.indexWhere(_.value == 0)
    List(1000, 2000, 3000).map(i => numbers((zero + i) % numbers.size).value)
  }

  private case class Number(value: Long, origIdx: Int)
}

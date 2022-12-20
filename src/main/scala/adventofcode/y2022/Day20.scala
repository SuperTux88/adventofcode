package adventofcode.y2022

import adventofcode.common.NumberHelper.ExtendedIntegral

import scala.collection.mutable.ArrayBuffer
import scala.io.BufferedSource

object Day20 extends Year2022 {
  override val day = 20

  private val DECRYPTION_KEY = 811589153L

  override def runDay(input: BufferedSource): Unit = {
    val numbers = input.getLines().zipWithIndex.map((l, i) => Number(l.toLong, i)).toVector

    val numBuffer = ArrayBuffer.from(numbers)
    numbers.foreach(n => moveNumberMutable(numBuffer, n))

    printDayPart(1, getGroveCoordinates(numBuffer).sum, "Sum of grove coordinates: %s")

    val decrypted = numbers.map(n => n.copy(value = n.value * DECRYPTION_KEY))
    val decryptedBuffer = ArrayBuffer.from(decrypted)
    (1 to 10).foreach { _ =>
      decrypted.foreach(n => moveNumberMutable(decryptedBuffer, n))
    }

    printDayPart(2, getGroveCoordinates(decryptedBuffer).sum, "Sum of grove coordinates with decription key: %s")
  }

  private def moveNumberMutable(numbers: ArrayBuffer[Number], number: Number): Unit = {
    val idx = numbers.indexWhere(_ == number)
    numbers.remove(idx)
    val insertIdx = (idx + number.value) %+ numbers.length
    numbers.insert(insertIdx.toInt, number)
  }

  private def getGroveCoordinates(numbers: ArrayBuffer[Number]): List[Long] = {
    val zero = numbers.indexWhere(_.value == 0)
    List(1000, 2000, 3000).map(i => numbers((zero + i) % numbers.size).value)
  }

  private case class Number(value: Long, origIdx: Int)
}

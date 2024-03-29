package adventofcode.y2020

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day25 extends Year2020 {
  override val day = 25

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()
    val cardsPublicKey = lines.next().toInt
    val doorsPublicKey = lines.next().toInt

    val doorsLoopSize = findLoopSize(doorsPublicKey)

    printDayPart(1, getEncryptionKey(cardsPublicKey, doorsLoopSize), "encryption key: %s")
  }

  @tailrec
  private def findLoopSize(targetKey: Int, loopSize: Int = 1, currentValue: Long = 1L): Int = {
    val newValue = transform(currentValue)
    if (newValue == targetKey)
      loopSize
    else
      findLoopSize(targetKey, loopSize + 1, newValue)
  }

  private def getEncryptionKey(subjectNumber: Int, loopSize: Int): Int =
    BigInt(subjectNumber).modPow(loopSize, 20201227).toInt

  private def transform(value: Long, subjectNumber: Int = 7): Long =
    value * subjectNumber % 20201227
}

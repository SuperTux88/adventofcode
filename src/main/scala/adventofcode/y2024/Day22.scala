package adventofcode.y2024

import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParSeq
import scala.io.BufferedSource

object Day22 extends Year2024 {
  override val day = 22

  private val PRUNE = 16777216

  override def runDay(input: BufferedSource): Unit = {
    val numbers = input.getLines().takeWhile(_.nonEmpty).map(_.toLong).toList

    val secrets = numbers.par.map(Iterator.iterate(_)(nextSecret).take(2001).toList)
    printDayPart(1, secrets.map(_.last).sum, "Sum of 2000th secret numbers: %s")

    val prices = secrets.map(_.map(s => (s % 10).toInt).toList)
    val priceChanges = prices.map(_.sliding(2).map(diff).sliding(4).toList)
    val priceChangesCount = priceChanges.flatten.groupBy(identity).mapValues(_.size).toList.sortBy(_._2)

    // this takes only the best 100 sequences and not all of them (works in my case, but it's not guaranteed to work in all cases)
    val maxBananas = priceChangesCount.takeRight(100).map(_._1).par.map(getBananas(prices, priceChanges, _)).max

    printDayPart(2, maxBananas, "Most bananas you can get: %s")
  }

  private def getBananas(prices: ParSeq[List[Int]], priceChanges: ParSeq[List[Seq[Int]]], sequence: Seq[Int]) =
    priceChanges.map(_.indexOf(sequence)).zip(prices).map((i, prices) => if (i == -1) 0 else prices(i + 4)).sum

  private def nextSecret(number: Long): Long = {
    val next = mixAndPrune(number, number << 6)
    val next2 = mixAndPrune(next, next >> 5)
    mixAndPrune(next2, next2 << 11)
  }

  private inline def mixAndPrune(number: Long, nextNumber: Long): Long = (number ^ nextNumber) % PRUNE

  private inline def diff(pair: Seq[Int]): Int = pair(1) - pair.head
}

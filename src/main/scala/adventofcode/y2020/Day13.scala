package adventofcode.y2020

import adventofcode.common.NumberHelper.chineseRemainder

object Day13 extends Year2020 {
  override val day = 13

  private val lines = input.getLines()
  private val earliestTimestamp = lines.next().toInt
  private val busses = lines.next().split(",").zipWithIndex.filter(_._1 != "x")
    .map(b => Bus(b._1.toLong, b._2)).toList

  private val (earliestBus, timeToWait) = busses.map(bus => bus -> (bus.id - earliestTimestamp % bus.id)).minBy(_._2)

  printDayPart(1, earliestBus.id * timeToWait, "earliest bus multiplied by time to wait: %s")

  private val crt = chineseRemainder(busses.map(_.id), busses.map(b => b.id - b.index))

  printDayPart(2, crt.get, "earliest timestamp with offset departs: %s")

  private case class Bus(id: Long, index: Long)
}

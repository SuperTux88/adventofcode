package adventofcode.y2016

import scala.io.BufferedSource

object Day20 extends Year2016 {
  override val day = 20

  override def runDay(input: BufferedSource): Unit = {
    val blacklist = input.getLines().map { line =>
      val range = line.split("-")
      range(0).toLong to range(1).toLong
    }.toList.sortBy(_.start)

    val (allowedIps, _) = blacklist.foldLeft(List.empty[Long], 0L) { (state, range) =>
      val (allowedIps, lastBlocked) = state
      val nextFree = lastBlocked + 1
      val nextLastBlocked = Math.max(lastBlocked, range.end)

      if (range.start > nextFree)
        (allowedIps ::: (nextFree until range.start).toList, nextLastBlocked)
      else
        (allowedIps, nextLastBlocked)
    }

    printDayPart(1, allowedIps.head)
    printDayPart(2, allowedIps.length)
  }
}

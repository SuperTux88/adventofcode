package adventofcode.y2023

import scala.collection.parallel.CollectionConverters.*

object Day5 extends Year2023 {
  override val day = 5

  private val SeedsRE = """seeds: ([\d ]+)""".r

  override def runDay(input: String): Unit = {
    val groups = input.split("\n\n").map(_.split('\n')).iterator

    val seeds = groups.next().map {
      case SeedsRE(seeds) => seeds.split(' ').map(_.toLong).toList
    }.head

    val rangeMaps = groups.map { group =>
      val title = group.head
      group.tail.map { line =>
        val Array(destinationRangeStart, sourceRangeStart, rangeLength) = line.split(' ').map(_.toLong)
        RangeMapping(title, destinationRangeStart, sourceRangeStart, rangeLength)
      }.toList
    }.toList

    val locations = seeds.map(findLocation(rangeMaps, _))

    printDayPart(1, locations.min, "Lowest location number: %s")

    val seedRanges = seeds.grouped(2).map { range =>
      (range: @unchecked) match {
        case List(start, length) => start until start + length
      }
    }.toList

    //    // This is very slow ... need to think about how to write this faster ... but brute force worked for now
    //    val minLocations = seedRanges.par.map { range =>
    //      range.foldLeft(Long.MaxValue) { (min, value) =>
    //        val location = findLocation(rangeMaps, value)
    //        if (location < min) location else min
    //      }
    //    }
    //
    //    printDayPart(2, minLocations.min, "Lowest location number with seed ranges: %s")
  }

  private def findLocation(rangeMaps: List[List[RangeMapping]], seed: Long): Long =
    rangeMaps.foldLeft(seed) { case (value, ranges) =>
      ranges.find(_.isInRange(value)).map(_.map(value)).getOrElse(value)
    }

  private case class RangeMapping(title: String, destinationRangeStart: Long, sourceRangeStart: Long, rangeLength: Long) {
    private val sourceRange = sourceRangeStart until sourceRangeStart + rangeLength

    def isInRange(value: Long): Boolean = sourceRange.contains(value)
    def map(value: Long): Long = value - sourceRangeStart + destinationRangeStart
  }
}

package adventofcode.y2023

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day12 extends Year2023 {
  override val day = 12

  override def runDay(input: BufferedSource): Unit = {
    val records = input.getLines().takeWhile(_.nonEmpty).map { line =>
      val Array(springsStr, groupsStr) = line.split(" ")
      val springs = springsStr.map {
        case '.' => Condition.Operational
        case '#' => Condition.Damaged
        case '?' => Condition.Unknown
      }.toList
      val damagedGroups = groupsStr.split(",").map(_.toInt).toList
      Record(springs, damagedGroups)
    }.toList.par

    printDayPart(1, records.map(_.getValidPossibilities).sum, "Sum of possible arrangements: %s")

    val records5 = records.map { r =>
      val springs = List.fill(5)(Condition.Unknown :: r.springs).flatten.tail
      val groups = List.fill(5)(r.damagedGroups).flatten
      Record(springs, groups)
    }
    printDayPart(2, records5.map(_.getValidPossibilities).sum, "Sum of possible arrangements after unfold: %s")
  }

  private enum Condition {
    case Operational, Damaged, Unknown
  }

  private case class Record(springs: List[Condition], damagedGroups: List[Int]) {
    def getValidPossibilities: Long = getCachedValidPossibilities(springs, damagedGroups)

    private val CACHE = mutable.Map.empty[(List[Condition], List[Int]), Long]

    private def getCachedValidPossibilities(springs: List[Condition], damagedGroups: List[Int]): Long =
      CACHE.getOrElseUpdate((springs, damagedGroups), (springs, damagedGroups) match {
        case (_, Nil) => if (springs.contains(Condition.Damaged)) 0 else 1 // only valid if no more damaged springs left
        case (Nil, _ :: _) => 0 // it no more springs left, but still groups left, it is invalid
        case (Condition.Operational :: tail, damagedGroups) => // skip operational springs
          getCachedValidPossibilities(tail, damagedGroups)
        case (Condition.Damaged :: _, currentGroup :: damagedGroups) =>
          if (springs.length >= currentGroup) { // try to consume all damaged springs in the current group
            val (group, tail) = springs.splitAt(currentGroup)
            if (group.contains(Condition.Operational)) 0 // if there are operational springs in the group, it is invalid
            else tail match {
              case Nil => if (damagedGroups.isEmpty) 1 else 0 // if there are no more groups left, it is valid
              case Condition.Damaged :: _ => 0 // if the next spring is also damaged, it is invalid
              case _ :: tail => getCachedValidPossibilities(tail, damagedGroups) // otherwise, continue with the next spring
            }
          } else 0 // if there are not enough springs left, it is invalid
        case (Condition.Unknown :: tail, damagedGroups) =>
          // try to consume the unknown spring as both operational and damaged
          getCachedValidPossibilities(tail, damagedGroups) + getCachedValidPossibilities(Condition.Damaged :: tail, damagedGroups)
      })
  }
}

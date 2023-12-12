package adventofcode.y2023

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day12 extends Year2023 {
  override val day = 12

  override def runDay(input: BufferedSource): Unit = {
    val records = input.getLines().takeWhile(_.nonEmpty).map { line =>
      val Array(conditionsStr, groupsStr) = line.split(" ")
      val conditions = conditionsStr.map {
        case '.' => Condition.Operational
        case '#' => Condition.Damaged
        case '?' => Condition.Unknown
      }.toList
      val damagedGroups = groupsStr.split(",").map(_.toInt).toList
      Record(conditions, damagedGroups)
    }.toList.par

    val valid = records.map(_.getValidPossibilities)
    printDayPart(1, valid.sum, "Sum of possible arrangements: %s")

    val records2 = records.map { r =>
      val conditions = List.fill(5)(Condition.Unknown :: r.conditions).flatten.tail
      val groups = List.fill(5)(r.damagedGroups).flatten
      Record(conditions, groups)
    }
    val valid2 = records2.map(_.getValidPossibilities)

    printDayPart(2, valid2.sum, "Sum of possible arrangements after unfold: %s")
  }

  private enum Condition {
    case Operational, Damaged, Unknown
  }

  private case class Record(conditions: List[Condition], damagedGroups: List[Int]) {
    def getValidPossibilities: Long = {
      val seen: mutable.Map[(Int, Int, Int), Long] = mutable.Map.empty

      def inner(conditionIndex: Int, groupIndex: Int, currentDamagedLength: Int): Long = {
        val key = (conditionIndex, groupIndex, currentDamagedLength)
        if (seen.contains(key)) return seen(key) // already known how many possibilities are possible with the rest

        if (conditionIndex == conditions.length) { // if last item
          if (groupIndex == damagedGroups.size && currentDamagedLength == 0)
            return 1 // valid if not damaged and if there are no more damaged groups left
          else if (groupIndex == damagedGroups.size - 1 && damagedGroups(groupIndex) == currentDamagedLength)
            return 1 // valid if the number of damaged items in the last group is correct
          else
            return 0 // invalid otherwise
        }

        var result = 0L
        val current = conditions(conditionIndex)

        if (current == Condition.Operational || current == Condition.Unknown) {
          if (currentDamagedLength == 0) {
            // we are in a group that is not damaged
            result += inner(conditionIndex + 1, groupIndex, 0)
          } else if (damagedGroups.lift(groupIndex).contains(currentDamagedLength)) {
            // the last group was damaged and the number of damaged items is correct
            result += inner(conditionIndex + 1, groupIndex + 1, 0)
          }
        }
        if (current == Condition.Damaged || current == Condition.Unknown) {
          // increase damaged counter
          result += inner(conditionIndex + 1, groupIndex, currentDamagedLength + 1)
        }

        seen += key -> result
        result
      }

      inner(0, 0, 0)
    }
  }
}

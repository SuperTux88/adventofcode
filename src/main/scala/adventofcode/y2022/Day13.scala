package adventofcode.y2022

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day13 extends Year2022 {
  override val day = 13

  override def runDay(input: BufferedSource): Unit = {
    val packets = input.getLines().filter(!_.isBlank).map(line => parsePackets(line.toList)).toList

    val sortedPairsIndices = packets.grouped(2).map(l => compareLists(l.head, l.last))
      .zipWithIndex.filter(_._1).map(_._2 + 1)

    printDayPart(1, sortedPairsIndices.sum, "Sum of indices of sorted pairs: %s")

    val dividers = List(List(List(2)), List(List(6)))
    val allPackets = (dividers ::: packets).sortWith(compareLists)

    printDayPart(2, dividers.map(allPackets.indexOf(_) + 1).product, "Decoder key: %s")
  }

  @tailrec
  private def parsePackets(packet: List[Char], stack: List[List[Any]] = Nil): List[Any] = packet match {
    case '[' :: remaining => parsePackets(remaining, List.empty :: stack)
    case ',' :: remaining => parsePackets(remaining, stack) // just drop commas
    case ']' :: remaining => stack match {
      case head :: current :: stack => parsePackets(remaining, (head.reverse :: current) :: stack)
      case current :: Nil => current.reverse
      case _ => throw new Exception("Invalid input, too many closing brackets, stack underflow")
    }
    case values => values.splitAt(values.indexWhere(!_.isDigit)) match {
      case (value, remaining) => parsePackets(remaining, (value.mkString.toInt :: stack.head) :: stack.tail)
    }
  }

  private def compareLists(list1: List[Any], list2: List[Any]) = {
    def compare(item1: Any, item2: Any): Int =
      (item1, item2) match {
        case (a: Int, b: Int) => a.compare(b)
        case (a: Int, b: List[Any]) => compare(List(a), b)
        case (a: List[Any], b: Int) => compare(a, List(b))
        case (Nil, _ :: _) => -1
        case (_ :: _, Nil) => 1
        case (Nil, Nil) => 0
        case (aHead :: aTail, bHead :: bTail) => compare(aHead, bHead) match {
          case 0 => compare(aTail, bTail)
          case x => x
        }
      }

    compare(list1, list2) == -1
  }
}

package adventofcode.y2021

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day18 extends Year2021 {
  override val day = 18

  override def runDay(input: BufferedSource): Unit = {
    val numbers = input.getLines().takeWhile(_.nonEmpty).map(line => parseNumber(line.toList)).toList

    printDayPart(1, numbers.reduce(reduceAll).magnitude, "magnitude of final sum: %s")

    val reducedPairs: List[SnailNumber] = (numbers.combinations(2) ++ numbers.reverse.combinations(2)).toList
      .par.map(_.reduce(reduceAll)).toList
    printDayPart(2, reducedPairs.map(_.magnitude).max, "largest magnitude of pairs: %s")
  }

  private def reduceAll(current: SnailNumber, next: SnailNumber): SnailNumber = reduceNumber(Pair(current, next))

  @tailrec
  private def reduceNumber(snailfishNumber: SnailNumber): SnailNumber =
    explode(snailfishNumber) match {
      case Some(_, exploded, _) => reduceNumber(exploded)
      case None =>
        split(snailfishNumber) match {
          case Some(splitted) => reduceNumber(splitted)
          case None => snailfishNumber
        }
    }

  private def explode(snailfishNumber: SnailNumber, depth: Int = 0): Option[(Option[Int], SnailNumber, Option[Int])] =
    snailfishNumber match {
      case Number(_) => None
      case Pair(Number(left), Number(right)) if depth >= 4 => Some(Some(left), Number(0), Some(right))
      case Pair(left, right) =>
        explode(left, depth + 1).map((addToLeft, newLeft, addToRight) =>
          (addToLeft, Pair(newLeft, addToRight.map(right.addToLeft).getOrElse(right)), None)
        ) orElse
          explode(right, depth + 1).map((addToLeft, newRight, addToRight) =>
            (None, Pair(addToLeft.map(left.addToRight).getOrElse(left), newRight), addToRight)
          )
    }

  private def split(snailfishNumber: SnailNumber): Option[SnailNumber] =
    snailfishNumber match {
      case Number(value) if value >= 10 =>
        val half = value / 2
        Some(Pair(Number(half), Number(half + value % 2)))
      case Number(_) => None
      case Pair(left, right) => split(left).map(Pair(_, right)) orElse split(right).map(Pair(left, _))
    }

  private def parseNumber(smailfishNumber: List[Char]): SnailNumber =
    smailfishNumber match {
      case '[' :: tail =>
        val (a, b) = tail.splitAt(indexOfPairComma(tail))
        Pair(parseNumber(a), parseNumber(b.tail))
      case number => Number(number.head.asDigit)
    }

  @tailrec
  private def indexOfPairComma(pair: List[Char], index: Int = 0, depth: Int = 0): Int =
    (pair: @unchecked) match {
      case '[' :: tail => indexOfPairComma(tail, index + 1, depth + 1)
      case ']' :: tail => indexOfPairComma(tail, index + 1, depth - 1)
      case ',' :: _ if depth == 0 => index
      case _ :: tail => indexOfPairComma(tail, index + 1, depth)
    }

  private sealed trait SnailNumber {
    def addToLeft(addValue: Int): SnailNumber
    def addToRight(addValue: Int): SnailNumber
    def magnitude: Int
  }
  private case class Number(value: Int) extends SnailNumber {
    override def addToLeft(addValue: Int): SnailNumber = Number(value + addValue)
    override def addToRight(addValue: Int): SnailNumber = Number(value + addValue)
    override def magnitude: Int = value
    override def toString: String = value.toString
  }
  private case class Pair(left: SnailNumber, right: SnailNumber) extends SnailNumber {
    override def addToLeft(addValue: Int): SnailNumber = copy(left = left.addToLeft(addValue))
    override def addToRight(addValue: Int): SnailNumber = copy(right = right.addToRight(addValue))
    override def magnitude: Int = left.magnitude * 3 + right.magnitude * 2
    override def toString: String = s"[$left,$right]"
  }
}

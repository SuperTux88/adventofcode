package adventofcode.y2021

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day18 extends Year2021 {
  override val day = 18

  private val SplitNumberRE = """(\d\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val numbers = input.getLines().takeWhile(_.nonEmpty).toList

    val reduced = numbers.reduce(reduceAll).toList
    printDayPart(1, calculateMagintude(reduced), "magnitude of final sum: %s")

    val reducedPairs: List[List[Char]] = (numbers.combinations(2) ++ numbers.reverse.combinations(2)).toList
      .par.map(_.reduce(reduceAll).toList).toList
    printDayPart(2, reducedPairs.map(calculateMagintude(_)).max, "largest magnitude of pairs: %s")
  }

  private def reduceAll(current: String, next: String): String = reduceNumber(s"[$current,$next]".toList).mkString

  @tailrec
  private def reduceNumber(snailfishNumber: List[Char]): List[Char] =
    findExplode(snailfishNumber) match {
      case Some(exploded) => reduceNumber(exploded)
      case None => findSplit(snailfishNumber.mkString) match {
        case Some(splitted) => reduceNumber(splitted.toList)
        case None => snailfishNumber
      }
    }

  @tailrec
  private def findExplode(right: List[Char], left: List[Char] = Nil, depth: Int = 0): Option[List[Char]] =
    right match {
      case '[' :: tail if depth == 4 =>
        val a = tail.takeWhile(_.isDigit)
        val b = tail.drop(a.size + 1).takeWhile(_.isDigit)
        val right = tail.drop(a.size + b.size + 2)
        Some(addToNumber(left, a.mkString.toInt, _.reverse) ::: List('0') ::: addToNumber(right, b.mkString.toInt))
      case '[' :: tail => findExplode(tail, '[' :: left, depth + 1)
      case ']' :: tail => findExplode(tail, ']' :: left, depth - 1)
      case head :: tail => findExplode(tail, head :: left, depth)
      case Nil => None
    }

  private def addToNumber(chars: List[Char], add: Int, order: List[Char] => List[Char] = identity): List[Char] =
    chars.indexWhere(_.isDigit) match {
      case -1 => chars
      case index =>
        val (left, right) = chars.splitAt(index)
        val insert = order((order(right.takeWhile(_.isDigit)).mkString.toInt + add).toString.toList)
        order(left ::: insert ::: right.dropWhile(_.isDigit))
    }

  private def findSplit(string: String): Option[String] =
    SplitNumberRE.findFirstMatchIn(string) match {
      case None => None
      case Some(number) =>
        val value = number.group(1).toInt
        val half = value / 2
        Some(s"${string.substring(0, number.start)}[${half},${half + value % 2}]${string.substring(number.end)}")
    }

  private def calculateMagintude(smailfishNumber: List[Char]): Int =
    smailfishNumber match {
      case '[' :: tail =>
        val (a, b) = tail.splitAt(indexOfPairComma(tail))
        calculateMagintude(a) * 3 + calculateMagintude(b.tail) * 2
      case number => number.head.asDigit
    }

  @tailrec
  private def indexOfPairComma(pair: List[Char], index: Int = 0, depth: Int = 0): Int =
    (pair: @unchecked) match {
      case '[' :: tail => indexOfPairComma(tail, index + 1, depth + 1)
      case ']' :: tail => indexOfPairComma(tail, index + 1, depth - 1)
      case ',' :: tail if depth == 0 => index
      case _ :: tail => indexOfPairComma(tail, index + 1, depth)
    }
}

package adventofcode.y2019

import scala.collection.parallel.CollectionConverters._

object Day4 extends Year2019 {
  override val day = 4

  private val Seq(min, max) = input.mkString.split("-").map(_.toInt).toSeq

  private val validPart1 = (min to max).par.map(_.toString.map(_.asDigit))
    .filter(pass => isIncreasing(pass) && hasGroup(pass))

  printDayPart(1, validPart1.size)
  printDayPart(2, validPart1.count(hasGroupOfExactlyTwo))

  private def isIncreasing(password: Seq[Int]) =
    password.sliding(2).forall { case Seq(a, b) => a <= b }

  private def hasGroup(password: Seq[Int]) =
    password.sliding(2).exists { case Seq(a, b) => a == b }

  private def hasGroupOfExactlyTwo(password: Seq[Int]) =
    password.groupBy(identity).exists(_._2.size == 2)
}

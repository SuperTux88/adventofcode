package adventofcode.y2024

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day5 extends Year2024 {
  override val day = 5

  private val RulesRE = """(\d+)\|(\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()
    val rules = lines.takeWhile(_.nonEmpty).map {
      case RulesRE(first, second) => Rule(first.toInt, second.toInt)
    }.toSeq
    val updates = lines.map(_.split(",").map(_.toInt).toSeq).toSeq

    val beforeRules = rules.groupMap(_.second)(_.first)
    val afterRules = rules.groupMap(_.first)(_.second)

    val (validOrders, invalidOrders) = updates.partition(findFirstBrokenIndex(beforeRules, afterRules)(_).isEmpty)
    printDayPart(1, validOrders.map(getMiddle).sum, "Sum of middle pages of correct updates: %s")

    val fixedOrders = invalidOrders.par.map(fixOrder(findFirstBrokenIndex(beforeRules, afterRules), _))
    printDayPart(2, fixedOrders.map(getMiddle).sum, "Sum of middle pages of fixed updates: %s")
  }

  private def findFirstBrokenIndex(beforeRules: Map[Int, Seq[Int]], afterRules: Map[Int, Seq[Int]])(update: Seq[Int]): Option[Int] =
    update.indices.find { i =>
      val page = update(i)
      val beforePages = update.take(i)
      val afterPages = update.drop(i + 1)
      beforeRules(page).exists(afterPages.contains) || afterRules(page).exists(beforePages.contains)
    }

  private def fixOrder(findBrokenIndex: Seq[Int] => Option[Int], update: Seq[Int]): Seq[Int] = {
    @tailrec
    def inner(update: Seq[Int]): Seq[Int] =
      findBrokenIndex(update) match {
        case None => update
        case Some(index) =>
          // TODO: There is probably a better way to find the next valid element
          //  this is more or less trial and error by just cycling the remaining elements until it's valid
          inner(update.take(index) ++ update.drop(index + 1) :+ update(index))
      }

    inner(update)
  }

  private def getMiddle(update: Seq[Int]): Int = update(update.length / 2)

  private case class Rule(first: Int, second: Int)
}

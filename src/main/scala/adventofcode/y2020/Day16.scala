package adventofcode.y2020

import adventofcode.common.MiscFunctions.reduceToUniqueValues

import scala.io.BufferedSource

object Day16 extends Year2020 {
  override val day = 16

  private val RuleRE = """([\w ]+): (\d+)-(\d+) or (\d+)-(\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()
    val rules = lines.takeWhile(_.nonEmpty).map {
      case RuleRE(name, a, b, c, d) => Rule(name, a.toInt to b.toInt, c.toInt to d.toInt)
    }.toList
    val myTicket = lines.drop(1).next().split(',').map(_.toLong).toList
    val nearbyTickets = lines.drop(2).map(_.split(',').map(_.toInt).toList).toList

    val errorRate = nearbyTickets.flatten.filter(value => !rules.exists(rule => rule.valid(value))).sum

    printDayPart(1, errorRate, "ticket scanning error rate: %s")

    val validTickets = nearbyTickets.filter(ticket => ticket.forall(value => rules.exists(_.valid(value))))
    val validForRows = rules.map { rule =>
      rule -> myTicket.indices.filter(index => validTickets.forall(ticket => rule.valid(ticket(index)))).toSet
    }.toMap
    val departureIndexes = reduceToUniqueValues(validForRows).filter(_._1.name.startsWith("departure")).values

    printDayPart(2, departureIndexes.map(myTicket(_)).product, "product of departure values: %s")
  }

  private case class Rule(name: String, rangeA: Range, rangeB: Range) {
    def valid(number: Int): Boolean = rangeA.contains(number) || rangeB.contains(number)
  }
}

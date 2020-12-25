package adventofcode.y2020

import adventofcode.common.MiscFunctions.reduceToUniqueValues

object Day16 extends Year2020 {
  override val day = 16

  private val RuleRE = """([\w ]+): (\d+)-(\d+) or (\d+)-(\d+)""".r

  private val lines = input.getLines()
  private val rules = lines.takeWhile(_.nonEmpty).map {
    case RuleRE(name, a, b, c, d) => Rule(name, a.toInt to b.toInt, c.toInt to d.toInt)
  }.toList
  private val myTicket = lines.drop(1).next().split(',').map(_.toLong).toList
  private val nearbyTickets = lines.drop(2).map(_.split(',').map(_.toInt).toList).toList

  private val errorRate = nearbyTickets.flatten.filter(value => !rules.exists(rule => rule.valid(value))).sum

  printDayPart(1, errorRate, "ticket scanning error rate: %s")

  private val validTickets = nearbyTickets.filter(ticket => ticket.forall(value => rules.exists(_.valid(value))))
  private val validForRows = rules.map { rule =>
    rule -> myTicket.indices.filter(index => validTickets.forall(ticket => rule.valid(ticket(index)))).toSet
  }.toMap
  private val departureIndexes = reduceToUniqueValues(validForRows).filter(_._1.name.startsWith("departure")).values

  printDayPart(2, departureIndexes.map(myTicket(_)).product, "product of departure values: %s")

  private case class Rule(name: String, rangeA: Range, rangeB: Range) {
    def valid(number: Int): Boolean = rangeA.contains(number) || rangeB.contains(number)
  }
}

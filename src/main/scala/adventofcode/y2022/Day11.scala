package adventofcode.y2022

import adventofcode.common.NumberHelper

object Day11 extends Year2022 {
  override val day = 11

  private val MonkeyRE =
    """Monkey (\d+):
      |  Starting items: ([\d, ]+)
      |  Operation: (new = old [+*] (?:\d+|old))
      |  Test: divisible by (\d+)
      |    If true: throw to monkey (\d+)
      |    If false: throw to monkey (\d+)""".stripMargin.r

  private val AddRE = """new = old \+ (\d+)""".r
  private val MultiRE = """new = old \* (\d+)""".r
  private val SquareRE = """new = old \* old""".r

  override def runDay(input: String): Unit = {
    val monkeys = input.split("\n\n").map {
      case MonkeyRE(id, itemsStr, operationStr, div, trueId, falseId) =>
        val items = itemsStr.split(", ").map(_.toLong).toList
        val operation = operationStr match {
          case AddRE(num) => (old: Long) => old + num.toInt
          case MultiRE(num) => (old: Long) => old * num.toInt
          case SquareRE() => (old: Long) => old * old
        }
        id.toInt -> Monkey(items, operation, div.toInt, (trueId.toInt, falseId.toInt))
    }.toMap

    val round20 = runRounds(monkeys, 20, value => value / 3)
    printDayPart(1, getLevelOfMonkeyBusiness(round20), "Level of monkey business after 20 rounds: %s")

    // By using the LCM of all possible divisors, the "divisible by" test can still be done, but it keeps the actual numbers low.
    // But as all my divisors are prime numbers anyway the LCM is actually just the same as the product of them, so maybe LCM isn't even needed?
    val divisibleByLCM = NumberHelper.lcm(monkeys.map(_._2.testDivisibleBy.toLong).toList)
    val round10000 = runRounds(monkeys, 10000, value => value % divisibleByLCM)
    printDayPart(2, getLevelOfMonkeyBusiness(round10000), "Level of monkey business after 10000 rounds: %s")
  }

  private def runRounds(monkeys: Map[Int, Monkey], rounds: Int, reduceWorryLevelOperation: Long => Long): Map[Int, Monkey] = {
    (1 to rounds).foldLeft(monkeys) { (monkeys, _) =>
      handleMonkeys(monkeys, reduceWorryLevelOperation)
    }
  }

  private def handleMonkeys(monkeys: Map[Int, Monkey], reduceWorryLevelOperation: Long => Long): Map[Int, Monkey] =
    monkeys.keys.toSeq.sorted.foldLeft(monkeys) { (monkeys, id) =>
      val (newMonkey, itemsToTarget) = monkeys(id).handleItems(reduceWorryLevelOperation)
      itemsToTarget.foldLeft(monkeys.updated(id, newMonkey)) {
        case (monkeys, (id, item)) => monkeys.updated(id, monkeys(id).addItem(item))
      }
    }

  private def getLevelOfMonkeyBusiness(monkeys: Map[Int, Monkey]): Long =
    monkeys.values.map(_.inspectedItems).toList.sorted.takeRight(2).product

  private case class Monkey(items: List[Long], operation: Long => Long, testDivisibleBy: Int, targets: (Int, Int), inspectedItems: Long = 0) {
    def handleItems(reduceWorryLevel: Long => Long): (Monkey, List[(Int, Long)]) = {
      val itemsToTargets = items.map { item =>
        val worryLevel = reduceWorryLevel(operation(item))
        val target = if worryLevel % testDivisibleBy == 0 then targets._1 else targets._2
        (target, worryLevel)
      }
      (copy(items = List.empty, inspectedItems = inspectedItems + items.size), itemsToTargets)
    }

    def addItem(item: Long): Monkey = copy(items = items :+ item)
  }
}

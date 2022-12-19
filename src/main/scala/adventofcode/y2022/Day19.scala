package adventofcode.y2022

import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day19 extends Year2022 {
  override val day = 19

  private val BlueprintRE = """Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.""".r

  override def runDay(input: BufferedSource): Unit = {
    val blueprints = input.getLines().map {
      case BlueprintRE(id, oreOreCost, clayOreCost, obsidianOreCost, obsidianClayCost, geodeOreCost, geodeObsidianCost) =>
        Blueprint(id.toInt,
          Costs(oreOreCost.toInt, clayOreCost.toInt, obsidianOreCost.toInt, obsidianClayCost.toInt, geodeOreCost.toInt, geodeObsidianCost.toInt)
        )
    }.toList

    val maxGeodes24 = blueprints.par.map(b => findBest(b) * b.id)
    printDayPart(1, maxGeodes24.sum, "Sum of all quality level: %s")

    val maxGeodes32 = blueprints.take(3).par.map(b => findBest(b, 32))
    println(maxGeodes32)
    printDayPart(2, maxGeodes32.product, "Largest numbers of geodes multiplied together: %s")
  }

  private def findBest(blueprint: Blueprint, minutes: Int = 24): Int =
    (1 to minutes).foldLeft(List(Resources())) { (states, _) =>
      val newStates = states.flatMap(_.canBuy(blueprint))
      (maxRes(newStates, _.clayBots), maxRes(newStates, _.obsidianBots), maxRes(newStates, _.geodeBots)) match {
        case (maxClay, 0, 0) => newStates.filter(_.clayBots >= maxClay - 1)
        case (_, maxObsidian, 0) => newStates.filter(_.obsidianBots >= maxObsidian - 1)
        case (_, _, maxGeode) => newStates.filter(_.geodeBots == maxGeode)
      }
    }.maxBy(_.geode).geode

  private def maxRes(res: List[Resources], by: Resources => Int) = by(res.maxBy(by))

  private case class Costs(oreOre: Int, clayOre: Int,
                           obsidianOre: Int, obsidianClay: Int,
                           geodeOre: Int, geodeObsidian: Int) {
    val maxOre: Int = Seq(oreOre, clayOre, obsidianOre, geodeOre).max
  }

  private case class Resources(ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geode: Int = 0,
                               oreBots: Int = 1, clayBots: Int = 0, obsidianBots: Int = 0, geodeBots: Int = 0) {
    def canBuy(blueprint: Blueprint): List[Resources] =
      buyGeodeBotOption(blueprint) match {
        case Some(geodeBot) => List(geodeBot)
        case None =>
          val buyBots = List(buyOreBotOption(blueprint), buyClayBotOption(blueprint), buyObsidianBotOption(blueprint))
          next :: buyBots.flatten
      }

    private def next = copy(ore = ore + oreBots, clay = clay + clayBots, obsidian = obsidian + obsidianBots, geode = geode + geodeBots)

    private def buyOreBotOption(blueprint: Blueprint) =
      if (ore >= blueprint.costs.oreOre && oreBots < blueprint.costs.maxOre)
        Some(next.buyOreBot(blueprint))
      else None

    private def buyOreBot(blueprint: Blueprint) = copy(ore = ore - blueprint.costs.oreOre, oreBots = oreBots + 1)

    private def buyClayBotOption(blueprint: Blueprint) =
      if (ore >= blueprint.costs.clayOre)
        Some(next.buyClayBot(blueprint))
      else None

    private def buyClayBot(blueprint: Blueprint) = copy(ore = ore - blueprint.costs.clayOre, clayBots = clayBots + 1)

    private def buyObsidianBotOption(blueprint: Blueprint) =
      if (ore >= blueprint.costs.obsidianOre && clay >= blueprint.costs.obsidianClay)
        Some(next.buyObsidianBot(blueprint))
      else None

    private def buyObsidianBot(blueprint: Blueprint) =
      copy(ore = ore - blueprint.costs.obsidianOre, clay = clay - blueprint.costs.obsidianClay, obsidianBots = obsidianBots + 1)

    private def buyGeodeBotOption(blueprint: Blueprint) =
      if (ore >= blueprint.costs.geodeOre && obsidian >= blueprint.costs.geodeObsidian)
        Some(next.buyGeodeBot(blueprint))
      else None

    private def buyGeodeBot(blueprint: Blueprint) =
      copy(ore = ore - blueprint.costs.geodeOre, obsidian = obsidian - blueprint.costs.geodeObsidian, geodeBots = geodeBots + 1)
  }

  private case class Blueprint(id: Int, costs: Costs)
}

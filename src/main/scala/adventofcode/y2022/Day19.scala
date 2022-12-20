package adventofcode.y2022

import scala.annotation.tailrec
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

    val qualityLevels = blueprints.par.map(b => findBest(b) * b.id)
    printDayPart(1, qualityLevels.sum, "Sum of all quality level: %s")

    val maxGeodes32 = blueprints.take(3).par.map(b => findBest(b, 32))
    printDayPart(2, maxGeodes32.product, "Largest numbers of geodes multiplied together: %s")
  }

  private def findBest(blueprint: Blueprint, minutes: Int = 24): Int = {
    @tailrec
    def find(states: List[State], best: Int = 0): Int =
      if (states.nonEmpty) {
        val newStates = states.flatMap(_.next(blueprint))
        val newBest = best max newStates.maxByOption(_.totalGeodes).map(_.totalGeodes).getOrElse(0)
        find(newStates.filter(_.canBeat(newBest)), newBest)
      } else best

    find(List(State(minutes)))
  }

  private case class Costs(oreOre: Int, clayOre: Int,
                           obsidianOre: Int, obsidianClay: Int,
                           geodeOre: Int, geodeObsidian: Int) {
    val maxOre: Int = Seq(oreOre, clayOre, obsidianOre, geodeOre).max
  }

  private case class Blueprint(id: Int, costs: Costs)

  private case class State(remaining: Int, totalGeodes: Int = 0,
                           ore: Int = 0, clay: Int = 0, obsidian: Int = 0,
                           oreBots: Int = 1, clayBots: Int = 0, obsidianBots: Int = 0) {

    // filter all states that can't beat the current best
    // (even if it would be possible to buy geodes every minute from now on)
    def canBeat(geodes: Int): Boolean = (1 to remaining).sum + totalGeodes >= geodes

    def next(blueprint: Blueprint): List[State] =
      List(oreBot(blueprint), clayBot(blueprint), obsidianBot(blueprint), geodeBot(blueprint)).flatten

    private def run(minutes: Int) = if (minutes < remaining) Some(copy(
      remaining = remaining - minutes,
      ore = ore + oreBots * minutes,
      clay = clay + clayBots * minutes,
      obsidian = obsidian + obsidianBots * minutes
    )) else None

    private def oreBot(b: Blueprint) = {
      if (oreBots < b.costs.maxOre && ore + oreBots * remaining > b.costs.oreOre) {
        val minutes = requiredMinutes(b.costs.oreOre, ore, oreBots)
        run(minutes + 1).map(s => s.copy(ore = s.ore - b.costs.oreOre, oreBots = oreBots + 1))
      } else None
    }

    private def clayBot(b: Blueprint) = {
      if (clayBots < b.costs.obsidianClay && ore + oreBots * remaining > b.costs.clayOre) {
        val minutes = requiredMinutes(b.costs.clayOre, ore, oreBots)
        run(minutes + 1).map(s => s.copy(ore = s.ore - b.costs.clayOre, clayBots = clayBots + 1))
      } else None
    }

    private def obsidianBot(b: Blueprint) = {
      if (obsidianBots < b.costs.geodeObsidian
        && ore + oreBots * remaining > b.costs.obsidianOre && clay + clayBots * remaining > b.costs.obsidianClay) {
        val oreMinutes = requiredMinutes(b.costs.obsidianOre, ore, oreBots)
        val clayMinutes = requiredMinutes(b.costs.obsidianClay, clay, clayBots)
        run((oreMinutes max clayMinutes) + 1).map(s => s.copy(
          ore = s.ore - b.costs.obsidianOre,
          clay = s.clay - b.costs.obsidianClay,
          obsidianBots = obsidianBots + 1
        ))
      } else None
    }

    private def geodeBot(b: Blueprint) = {
      if (ore + oreBots * remaining > b.costs.geodeOre && obsidian + obsidianBots * remaining > b.costs.geodeObsidian) {
        val oreMinutes = requiredMinutes(b.costs.geodeOre, ore, oreBots)
        val obsidianMinutes = requiredMinutes(b.costs.geodeObsidian, obsidian, obsidianBots)
        val minutes = (oreMinutes max obsidianMinutes) + 1
        run(minutes).map(s => s.copy(
          ore = s.ore - b.costs.geodeOre,
          obsidian = s.obsidian - b.costs.geodeObsidian,
          totalGeodes = totalGeodes + (remaining - minutes)
        ))
      } else None
    }

    private def requiredMinutes(cost: Int, available: Int, bots: Int) =
      if (available < cost) {
        val requiredOre = cost - available
        requiredOre / bots + (if requiredOre % bots == 0 then 0 else 1)
      } else 0
  }
}

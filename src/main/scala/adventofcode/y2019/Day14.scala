package adventofcode.y2019

import scala.annotation.tailrec

object Day14 extends Year2019 {
  override val day = 14

  private val ChemicalRE = """(\d+) (\w+)""".r
  private val ReactionRE = """(.+) => (\d+) (\w+)""".r

  private val chemicals = input.getLines().map {
    case ReactionRE(input, quantity, output) =>
      output -> Chemical(output, quantity.toInt, input.split(", ").map {
        case ChemicalRE(quantity, name) => name -> quantity.toInt
      }.toMap)
  }.toMap

  private val neededForOneFuel = oreForFuel(1)
  printDayPart(1, neededForOneFuel)

  private val worstCaseResult = 1000000000000L / neededForOneFuel
  printDayPart(2, findMaximumFuel(worstCaseResult, worstCaseResult * 2))

  private def oreForFuel(fuel: Long) = react(Map("FUEL" -> fuel))

  @tailrec
  private def react(needed: Map[String, Long], storage: Map[String, Long] = Map.empty.withDefaultValue(0)): Long = {
    if (needed.keys.toSeq == Seq("ORE")) {
      needed("ORE")
    } else {
      val (nextNeededChemical, nextNeededQuantity) = needed.find(_._1 != "ORE").get
      if (storage(nextNeededChemical) >= nextNeededQuantity) {
        react(needed - nextNeededChemical, storage.updated(nextNeededChemical, storage(nextNeededChemical) - nextNeededQuantity))
      } else {
        val neededQuantity = nextNeededQuantity - storage(nextNeededChemical)
        val chemical = chemicals(nextNeededChemical)
        val reactionQuantity = math.ceil(neededQuantity.toDouble / chemical.quantity).toLong
        val neededInputs = chemical.reaction.view.mapValues(reactionQuantity * _)
        val leftover = reactionQuantity * chemical.quantity + storage(nextNeededChemical) - nextNeededQuantity

        val neededSum = needed ++ neededInputs.map { case (k, v) => k -> (v + needed.getOrElse(k, 0L)) }
        react(neededSum - nextNeededChemical, storage.updated(nextNeededChemical, leftover))
      }
    }
  }

  @tailrec
  private def findMaximumFuel(low: Long, high: Long): Long =
    if (low < high) {
      val mid = (low + high + 1) / 2
      if (oreForFuel(mid) <= 1000000000000L)
        findMaximumFuel(mid, high)
      else
        findMaximumFuel(low, mid - 1)
    } else {
      low
    }

  private case class Chemical(name: String, quantity: Int, reaction: Map[String, Int])
}

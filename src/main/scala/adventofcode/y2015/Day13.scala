package adventofcode.y2015

import scala.io.BufferedSource

object Day13 extends Year2015 {
  override val day: Int = 13

  private val HappinessRE = """(\w+) would (lose|gain) (\d+) happiness units by sitting next to (\w+).""".r

  override def runDay(input: BufferedSource): Unit = {
    val guestsHappiness = input.getLines().map {
      case HappinessRE(a, loseGain, happiness, b) =>
        (a, b) -> (if (loseGain == "lose") happiness.toInt * -1 else happiness.toInt)
    }.toMap.withDefaultValue(0)

    val guests = guestsHappiness.flatMap {
      case ((a, b), _) => Seq(a, b)
    }.toList.distinct

    printDayPart(1, calculateBestHappinessChangeForGuests(guests, guestsHappiness), "best happiness change: %s")
    printDayPart(2, calculateBestHappinessChangeForGuests("Me" :: guests, guestsHappiness), "best happiness change including me: %s")
  }

  private def calculateBestHappinessChangeForGuests(people: List[String], guestsHappiness: Map[(String, String), Int]) =
    people.permutations.map { arrangement =>
      (arrangement.last :: arrangement).sliding(2).map { x =>
        (x: @unchecked) match {
          case Seq(a, b) => guestsHappiness(a, b) + guestsHappiness(b, a)
        }
      }.sum
    }.toSeq.max
}

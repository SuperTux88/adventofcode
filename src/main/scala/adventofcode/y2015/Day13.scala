package adventofcode.y2015

object Day13 extends Year2015 {
  override val day: Int = 13

  val HappinessRE = """(\w+) would (lose|gain) (\d+) happiness units by sitting next to (\w+).""".r

  val guestsHappiness = input.getLines.map {
    case HappinessRE(a, loseGain, happiness, b) =>
      (a, b) -> (if (loseGain == "lose") happiness.toInt * -1 else happiness.toInt)
  }.toMap.withDefaultValue(0)

  val guests = guestsHappiness.flatMap {
    case ((a, b), _) => Seq(a, b)
  }.toList.distinct

  printDayPart(1, calculateBestHappinessChangeForGuests(guests), "best happiness change: %s")
  printDayPart(2, calculateBestHappinessChangeForGuests("Me" :: guests), "best happiness change including me: %s")

  private def calculateBestHappinessChangeForGuests(people: List[String]) =
    people.permutations.map { arrangement =>
      (arrangement.last :: arrangement).sliding(2).map {
        case Seq(a, b) => guestsHappiness(a, b) + guestsHappiness(b, a)
      }.sum
    }.toSeq.max
}

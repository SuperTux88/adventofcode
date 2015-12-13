package adventofcode

object Day13 extends DayApp {
  override val day: Int = 13

  val HappinessRE = """(\w+) would (lose|gain) (\d+) happiness units by sitting next to (\w+).""".r

  val guestsHappiness = input.getLines().map {
    case HappinessRE(a, loseGain, happiness, b) =>
      (a, b) -> (if (loseGain == "lose") happiness.toInt * -1 else happiness.toInt)
  }.toMap

  val guests = guestsHappiness.flatMap {
    case ((a, b), _) => Seq(a, b)
  }.toList.distinct

  printDayPart(1, s"best happiness change: ${calculateBestHappinessChangeForGuests(guests)}")
  printDayPart(2, s"best happiness change including me: ${calculateBestHappinessChangeForGuests("Me" :: guests)}")

  private def calculateBestHappinessChangeForGuests(people: List[String]) =
    people.permutations.map { arrangement =>
      (arrangement.last :: arrangement).sliding(2).map {
        case Seq(a, b) => guestsHappiness.getOrElse((a, b), 0) + guestsHappiness.getOrElse((b, a), 0)
      }.sum
    }.toSeq.max
}

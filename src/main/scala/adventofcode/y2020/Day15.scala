package adventofcode.y2020

object Day15 extends Year2020 {
  override val day = 15

  private val startingNumbers = inputString.split(',').map(_.toInt).toSeq

  printDayPart(1, play(2020), "2020th spoken number: %s")
  printDayPart(2, play(30000000), "30000000th spoken number: %s")

  private def play(rounds: Int): Int =
    (startingNumbers.size - 1 until rounds - 1)
      .foldLeft(startingNumbers.init.zipWithIndex.toMap, startingNumbers.last) { (spoken, round) =>
        val (lastSpokenMap, current) = spoken
        (lastSpokenMap.updated(current, round), round - lastSpokenMap.getOrElse(current, round))
      }._2
}

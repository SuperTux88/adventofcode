package adventofcode.y2020

object Day15 extends Year2020 {
  override val day = 15

  override def runDay(input: String): Unit = {
    val startingNumbers = input.split(',').map(_.toInt).toSeq

    printDayPart(1, play(startingNumbers, 2020), "2020th spoken number: %s")
    printDayPart(2, play(startingNumbers, 30000000), "30000000th spoken number: %s")
  }

  private def play(startingNumbers: Seq[Int], rounds: Int): Int = {
    val numbersMap = Array.fill(rounds)(-1)
    startingNumbers.init.zipWithIndex.foreach(num => numbersMap(num._1) = num._2 + 1)
    (startingNumbers.size until rounds).foldLeft(startingNumbers.last) { (current, round) =>
      val lastIndex = numbersMap(current)
      numbersMap(current) = round
      if (lastIndex == -1) 0 else round - lastIndex
    }
  }
}

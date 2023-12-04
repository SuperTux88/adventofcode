package adventofcode.y2023

import scala.collection.immutable.ArraySeq
import scala.io.BufferedSource

object Day4 extends Year2023 {
  override val day = 4

  private val CardRE = """Card +(\d+): ([\d ]+) \| ([\d ]+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val cards = input.getLines().takeWhile(_.nonEmpty).map {
      case CardRE(id, winningNumbers, myNumbers) =>
        Card(id.toInt, parseNumbers(winningNumbers), parseNumbers(myNumbers))
    }.toSeq

    printDayPart(1, cards.map(_.score).sum, "Total score: %s")

    val totalCards = cards.foldLeft(Map.empty[Int, Int])((cardsCount, card) =>
      val currentCard = cardsCount.getOrElse(card.id, 0) + 1
      val currentCardsCount = cardsCount.updated(card.id, currentCard)
      card.cardsWon.foldLeft(currentCardsCount)((total, card) =>
        total.updated(card, total.getOrElse(card, 0) + currentCard)
      )
    )

    printDayPart(2, totalCards.values.sum, "Total number of scratchcards: %s")
  }

  private def parseNumbers(str: String): Seq[Int] =
    ArraySeq.unsafeWrapArray(str.trim.split("\\s+")).map(_.toInt)

  private case class Card(id: Int, winningNumbers: Seq[Int], myNumbers: Seq[Int]) {
    private val winningCount: Int = winningNumbers.count(myNumbers.contains)

    def score: Int =
      if (winningCount == 0) 0 else math.pow(2, winningCount - 1).toInt

    def cardsWon: Seq[Int] =
      if (winningCount == 0) Seq.empty else (1 to winningCount).map(id + _)
  }
}

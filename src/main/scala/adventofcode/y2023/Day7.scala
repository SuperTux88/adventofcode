package adventofcode.y2023

import scala.io.BufferedSource

object Day7 extends Year2023 {
  override val day = 7

  private val VALUE_ORDER = "X23456789TJQKA"

  override def runDay(input: BufferedSource): Unit = {
    val hands = input.getLines().takeWhile(_.nonEmpty).map { line =>
      val Seq(cards, bid) = line.split(" ").toSeq
      Hand(cards, bid.toInt)
    }.toSeq

    val scores = getScores(hands)
    printDayPart(1, scores.sum, "Total winnings: %s")

    val scores2 = getScores(hands.map(_.withJokers))
    printDayPart(2, scores2.sum, "Total winnings with jokers: %s")
  }

  private def getScores(hands: Seq[Hand]): Seq[Long] =
    hands.sorted.zipWithIndex.map((h, i) => h.bid.toLong * (i + 1))

  private case class Hand(cards: String, bid: Int) extends Ordered[Hand] {
    private val grouped = cards.groupBy(identity)
    private val handType = getType
    private val cardValues = cards.map(VALUE_ORDER.indexOf(_))

    def withJokers: Hand = copy(cards = cards.replace('J', 'X'))

    private def getType = {
      grouped.size match {
        case 1 => Type.FiveOfAKind
        case 2 =>
          if (cards.contains('X'))
            Type.FiveOfAKind
          else if (grouped.values.exists(_.length == 4))
            Type.FourOfAKind
          else
            Type.FullHouse
        case 3 =>
          if (grouped.values.exists(_.length == 3)) {
            if (cards.contains('X'))
              Type.FourOfAKind
            else
              Type.ThreeOfAKind
          } else {
            cards.count(_ == 'X') match {
              case 2 => Type.FourOfAKind
              case 1 => Type.FullHouse
              case _ => Type.TwoPair
            }
          }
        case 4 => if (cards.contains('X')) Type.ThreeOfAKind else Type.OnePair
        case _ => if (cards.contains('X')) Type.OnePair else Type.HighCard
      }
    }

    override def compare(that: Hand): Int =
      handType.value.compare(that.handType.value) match {
        case 0 => cardValues.zip(that.cardValues).map(_.compare(_)).find(_ != 0).getOrElse(0)
        case c => c
      }
  }

  private enum Type(val value: Int) {
    case HighCard extends Type(1)
    case OnePair extends Type(2)
    case TwoPair extends Type(3)
    case ThreeOfAKind extends Type(4)
    case FullHouse extends Type(5)
    case FourOfAKind extends Type(6)
    case FiveOfAKind extends Type(7)
  }
}

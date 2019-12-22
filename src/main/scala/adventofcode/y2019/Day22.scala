package adventofcode.y2019

object Day22 extends Year2019 {
  override val day = 22

  private val DealIntoNewStackRE = "deal into new stack"
  private val CutRE = """cut (-?\d+)""".r
  private val DealWithIncrementRE = """deal with increment (\d+)""".r

  private val instructions = input.getLines().takeWhile(_.nonEmpty).map {
    case DealIntoNewStackRE => DealIntoNewStack()
    case CutRE(n) => Cut(n.toInt)
    case DealWithIncrementRE(n) => DealWithIncrement(n.toInt)
  }.toSeq

  private val DECK_SIZE_1 = 10007
  private val finalDeck = instructions.foldLeft(Vector.range(0, DECK_SIZE_1)) { (currentDeck, instruction) =>
    instruction.shuffle(currentDeck)
  }

  printDayPart(1, finalDeck.indexOf(2019), "position of card 2019: %s")

  sealed trait ShuffleTechnique {
    def shuffle(deck: Vector[Int]): Vector[Int]
  }
  private case class DealIntoNewStack() extends ShuffleTechnique {
    override def shuffle(deck: Vector[Int]): Vector[Int] = deck.reverse
  }
  private case class Cut(n: Int) extends ShuffleTechnique {
    override def shuffle(deck: Vector[Int]): Vector[Int] = {
      val index = (deck.length + n) % deck.length
      val (init, tail) = deck.splitAt(index)
      tail ++ init
    }
  }
  private case class DealWithIncrement(n: Int) extends ShuffleTechnique {
    override def shuffle(deck: Vector[Int]): Vector[Int] =
      deck.indices.foldLeft(deck) { (currentDeck, index) =>
        currentDeck.updated(index * n % deck.length, deck(index))
      }
  }
}

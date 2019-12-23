package adventofcode.y2019

import adventofcode.common.NumberHelper.modInv
import adventofcode.common.NumberHelper.ExtendedLong

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

  private val DECK_SIZE_2 = 119315717514047L
  private val SHUFFLE_REPEAT = 101741582076661L
  private val TARGET_POS = 2020

  // get offsets for one reverse iteration: card == a * targetPos + b
  val (a, b) = instructions.reverse.foldLeft((1L, 0L)) {
    case ((a, b), instruction) => instruction.reversePosLinear(a, b, DECK_SIZE_2)
  } match {
    case (a, b) => (a %+ DECK_SIZE_2, b %+ DECK_SIZE_2)
  }

  // get card after multiple iterations
  // second iteration: card == a * (a * targetPos + b) + b
  // third iteration:  card == a * (a * (a * targetPos + b) + b) + b == a^3 * targetPos + a^2*b + a*b + b
  // (after every iteration its a * "the result of the previous" + b)

  // final card after n iteration:
  // a^n * targetPos + sum of "b*a^i for i=0 to i=n-1"
  //                   https://www.wolframalpha.com/input/?i=+%5Csum+b*a%5Ei+for+i%3D0+to+i%3Dn-1
  // a^n * targetPos + (((a^n)-1)*b)/(a-1)

  val cardAtPos2020 = (
    (BigInt(a).modPow(SHUFFLE_REPEAT, DECK_SIZE_2) * TARGET_POS +
      // because we use modPow we can multiply by modInv of a-1 instead of dividing by 1-a
      (BigInt(a).modPow(SHUFFLE_REPEAT, DECK_SIZE_2) - 1) * b * modInv(a - 1, DECK_SIZE_2)) % DECK_SIZE_2
    ).toLong %+ DECK_SIZE_2

  printDayPart(2, cardAtPos2020, "card at position 2020: %s")

  sealed trait ShuffleTechnique {
    def shuffle(deck: Vector[Int]): Vector[Int]

    def reversePosLinear(a: Long, b: Long, size: Long): (Long, Long)
  }
  private case class DealIntoNewStack() extends ShuffleTechnique {
    override def shuffle(deck: Vector[Int]): Vector[Int] = deck.reverse
    override def reversePosLinear(a: Long, b: Long, size: Long): (Long, Long) = (-a, size - 1 - b)
  }
  private case class Cut(n: Int) extends ShuffleTechnique {
    override def shuffle(deck: Vector[Int]): Vector[Int] = {
      val (init, tail) = deck.splitAt((n %+ deck.length).toInt)
      tail ++ init
    }
    override def reversePosLinear(a: Long, b: Long, size: Long): (Long, Long) = (a, b + n)
  }
  private case class DealWithIncrement(n: Int) extends ShuffleTechnique {
    override def shuffle(deck: Vector[Int]): Vector[Int] = {
      Vector.tabulate(deck.length)(i => deck(reversePos(i, deck.length).toInt))
    }
    override def reversePosLinear(a: Long, b: Long, size: Long): (Long, Long) = {
      val nInv = nModInv(size)
      (((BigInt(a) * nInv) % size).toLong, ((BigInt(b) * nInv) % size).toLong)
    }

    private def reversePos(index: Long, size: Long): Long = nModInv(size) * index % size
    private def nModInv(size: Long) = {
      modInv(n, size) %+ size
    }
  }
}

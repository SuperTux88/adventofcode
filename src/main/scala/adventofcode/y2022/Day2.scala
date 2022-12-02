package adventofcode.y2022

import scala.io.BufferedSource

object Day2 extends Year2022 {
  override val day = 2

  override def runDay(input: BufferedSource): Unit = {
    val strategy = input.getLines().map { line =>
      (parseShape(line.charAt(0)), parseShape(line.charAt(2)), parseOutcome(line.charAt(2)))
    }.toSeq

    val scores = strategy.map {
      case (opponentShape, myShape, _) =>
        if (opponentShape == myShape) {
          myShape.score + Outcome.Draw.score
        } else if (beats(opponentShape) == myShape) {
          myShape.score + Outcome.Win.score
        } else {
          myShape.score + Outcome.Lose.score
        }
    }
    printDayPart(1, scores.sum, "Total score when following strategy: %s")

    val scores2 = strategy.map {
      case (opponentShape, _, Outcome.Draw) =>
        opponentShape.score + Outcome.Draw.score
      case (opponentShape, _, Outcome.Win) =>
        beats(opponentShape).score + Outcome.Win.score
      case (opponentShape, _, Outcome.Lose) =>
        isBeatenBy(opponentShape).score + Outcome.Lose.score
    }
    printDayPart(2, scores2.sum, "Total score when following elf's strategy: %s")
  }

  private def parseShape(shape: Char): Shape = shape match {
    case 'A'|'X' => Shape.Rock
    case 'B'|'Y' => Shape.Paper
    case 'C'|'Z' => Shape.Scissors
  }

  private def parseOutcome(outcome: Char): Outcome = outcome match {
    case 'X' => Outcome.Lose
    case 'Y' => Outcome.Draw
    case 'Z' => Outcome.Win
  }

  private val isBeatenBy = Map(
    Shape.Rock -> Shape.Scissors,
    Shape.Paper -> Shape.Rock,
    Shape.Scissors -> Shape.Paper
  )
  private val beats = isBeatenBy.map(_.swap)

  private enum Shape(val score: Int) {
    case Rock extends Shape(1)
    case Paper extends Shape(2)
    case Scissors extends Shape(3)
  }

  private enum Outcome(val score: Int) {
    case Win extends Outcome(6)
    case Draw extends Outcome(3)
    case Lose extends Outcome(0)
  }
}

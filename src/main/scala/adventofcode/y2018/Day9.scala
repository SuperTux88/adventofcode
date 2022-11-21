package adventofcode.y2018

import scala.annotation.{tailrec, unused}

object Day9 extends Year2018 {
  override val day = 9

  private val InputRE = """(\d+) players; last marble is worth (\d+) points""".r

  override def runDay(input: String): Unit = {
    val (players, lastMarble) = input match {
      case InputRE(playersStr, lastMarbleStr) => (playersStr.toInt, lastMarbleStr.toInt)
    }

    printDayPart(1, play(players, lastMarble).maxBy(_._2)._2)
    printDayPart(2, play(players, lastMarble * 100).maxBy(_._2)._2)
  }

  def play(players: Int, lastMarble: Int): Map[Int, Long] = {
    @tailrec
    def round(circle: MarbleCircle, currentMarble: Int, score: Map[Int, Long]): Map[Int, Long] =
      if (currentMarble > lastMarble) {
        score
      } else if (currentMarble % 23 != 0) {
        round(circle.moveClockwiseAndInsert(currentMarble), currentMarble + 1, score)
      } else {
        val (newCircle, removedMarble) = circle.move7StepsCounterClockwiseAndRemove
        val currentPlayer = currentMarble % players
        val newPlayerScore = score(currentPlayer) + currentMarble + removedMarble
        round(newCircle, currentMarble + 1, score + (currentPlayer -> newPlayerScore))
      }

    round(MarbleCircle(Nil, 0, Nil), 1, Map().withDefaultValue(0))
  }

  private case class MarbleCircle(before: List[Int], current: Int, after: List[Int]) {
    def moveClockwiseAndInsert(marble: Int): MarbleCircle =
      after match {
        case head :: tail => MarbleCircle(head :: current :: before, marble, tail)
        case Nil => before.reverse match {
          case newCurrent :: tail => MarbleCircle(List(newCurrent, current), marble, tail)
          case Nil => MarbleCircle(List(current), marble, after)
        }
      }

    def move7StepsCounterClockwiseAndRemove: (MarbleCircle, Int) =
      (0 until 6).foldLeft(this) { (circle, _) =>
        circle.before match {
          case head :: tail => MarbleCircle(tail, head, circle.current :: circle.after)
          case Nil => circle.after.reverse match {
            case newCurrent :: newBefore => MarbleCircle(newBefore, newCurrent, List(circle.current))
            case Nil => this
          }
        }
      } match {
        case MarbleCircle(remove :: newBefore, newCurrent, newAfter) => (MarbleCircle(newBefore, newCurrent, newAfter), remove)
        case MarbleCircle(Nil, newAfter, oldAfter) => oldAfter.reverse match {
          case remove :: newCurrent :: newBefore => (MarbleCircle(newBefore, newCurrent, List(newAfter)), remove)
          case _ => throw new IllegalStateException("This should never happen!")
        }
      }

    @unused
    def print(): Unit = printDebug(s"${before.reverse.mkString(" ")} ($current) ${after.mkString(" ")}")
  }
}

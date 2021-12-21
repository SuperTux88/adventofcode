package adventofcode.y2021

import adventofcode.common.IterableImplicits
import adventofcode.common.MapImplicits.IntegralMapImplicits

import scala.annotation.tailrec

object Day21 extends Year2021 {
  override val day = 21

  private val PlayersRE =
    """Player 1 starting position: (\d+)
      |Player 2 starting position: (\d+)""".stripMargin.r

  private val quantumDice = (for (a <- 1 to 3; b <- 1 to 3; c <- 1 to 3) yield List(a, b, c)).groupCount(_.sum)

  override def runDay(input: String): Unit = {
    val players = input match {
      case PlayersRE(p1, p2) => (Player(p1.toInt), Player(p2.toInt))
    }

    val testDice = Iterator.iterate(1)(_ % 100 + 1)
    printDayPart(1, playTest(players, testDice))
    printDayPart(2, playQuantum(Map(players -> 1L)), "number of universes the winning player wins: %s")
  }

  @tailrec
  private def playTest(players: (Player, Player), dice: Iterator[Int], rolled: Int = 0): Int = {
    val (currentPlayer, otherPlayer) = players
    val newPlayer = currentPlayer.move(dice.take(3).sum)
    if (newPlayer.score >= 1000)
      otherPlayer.score * (rolled + 3)
    else
      playTest((otherPlayer, newPlayer), dice, rolled + 3)
  }

  @tailrec
  private def playQuantum(states: Map[(Player, Player), Long], winCount: (Long, Long) = (0L, 0L)): Long = {
    val newStates = states.foldLeft(Map[(Player, Player), Long]().withDefaultValue(0L)) {
      case (newStates, ((currentPlayer, otherPlayer), currentStateCount)) =>
        quantumDice.foldLeft(newStates) { case (newStates, (steps, count)) =>
          val newPlayers = (otherPlayer, currentPlayer.move(steps))
          newStates.changeBy(newPlayers, currentStateCount * count)
        }
    }
    val (wonStates, stillRunning) = newStates.partition { case ((_, currentPlayer), _) => currentPlayer.score >= 21 }
    val newWinCount = (winCount._2, winCount._1 + wonStates.values.sum)
    if (stillRunning.isEmpty)
      math.max(newWinCount._1, newWinCount._2)
    else
      playQuantum(stillRunning, newWinCount)
  }

  private case class Player(pos: Int, score: Int = 0) {
    def move(steps: Int): Player = {
      val target = (pos + steps - 1) % 10 + 1
      copy(pos = target, score = score + target)
    }
  }
}

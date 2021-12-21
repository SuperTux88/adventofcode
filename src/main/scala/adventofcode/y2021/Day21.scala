package adventofcode.y2021

import adventofcode.common.MapImplicits.IntegralMapImplicits

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day21 extends Year2021 {
  override val day = 21

  private val PlayerRE = """Player (\d+) starting position: (\d+)""".r

  private val quantumDice = (for (a <- 1 to 3; b <- 1 to 3; c <- 1 to 3) yield List(a, b, c)).toList

  override def runDay(input: BufferedSource): Unit = {
    val List(player1, player2) = input.getLines().takeWhile(_.nonEmpty).map {
      case PlayerRE(i, pos) => Player(i.toInt, pos.toInt)
    }.toList

    val testDice = Iterator.iterate(1)(_ % 100 + 1)
    printDayPart(1, playTest((player1, player2), testDice))
    printDayPart(2, playQuantum(Map((player1, player2) -> 1L)), "number of universes the winning player wins: %s")
  }

  @tailrec
  private def playTest(players: (Player, Player), dice: Iterator[Int], rolled: Int = 0): Int = {
    val (currentPlayer, otherPlayer) = players
    val newPlayer = currentPlayer.move(dice.take(3).toList)
    if (newPlayer.score >= 1000)
      otherPlayer.score * (rolled + 3)
    else
      playTest((otherPlayer, newPlayer), dice, rolled + 3)
  }

  @tailrec
  private def playQuantum(states: Map[(Player, Player), Long], winCount: (Long, Long) = (0L, 0L)): Long = {
    val newStates = states.foldLeft(Map[(Player, Player), Long]().withDefaultValue(0L)) {
      case (newStates, ((currentPlayer, otherPlayer), currentStateCount)) =>
        quantumDice.foldLeft(newStates) { (newStates, dice) =>
          val newPlayers = (otherPlayer, currentPlayer.move(dice))
          newStates.changeBy(newPlayers, currentStateCount)
        }
    }
    val (wonStates, stillRunning) = newStates.partition { case ((_, currentPlayer), _) => currentPlayer.score >= 21 }
    val newWinCount = (winCount._2, winCount._1 + wonStates.values.sum)
    if (stillRunning.isEmpty)
      math.max(newWinCount._1, newWinCount._2)
    else
      playQuantum(stillRunning, newWinCount)
  }

  private case class Player(i: Int, pos: Int, score: Int = 0) {
    def move(moves: List[Int]) = {
      val target = (moves.foldLeft(pos)(_ + _) - 1) % 10 + 1
      copy(pos = target, score = score + target)
    }
  }
}

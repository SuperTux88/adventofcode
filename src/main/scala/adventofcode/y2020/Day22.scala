package adventofcode.y2020

import scala.annotation.tailrec

object Day22 extends Year2020 {
  override val day = 22

  private val players = inputString.split("\n\n").zipWithIndex.map { player =>
    Player(player._2 + 1, player._1.linesIterator.drop(1).map(_.toInt).toVector)
  }.toSet

  printDayPart(1, getScore(play(players)), "score of winner: %s")
  printDayPart(2, getScore(playSubGames(players)), "score of winner with recursive games: %s")

  @tailrec
  private def play(players: Set[Player]): Player =
    if (players.exists(_.deck.isEmpty)) {
      players.find(_.deck.nonEmpty).get
    } else {
      val cards = players.map(p => p.number -> p.nextCard).toMap
      val (winner, _) = cards.maxBy(_._2)
      play(players.map(p => if (p.number == winner) p.win(cards) else p.loose))
    }

  private def playSubGames(players: Set[Player], playersStates: Set[Set[Player]] = Set.empty): Player =
    if (playersStates.contains(players)) {
      players.find(_.number == 1).get
    } else if (players.exists(_.deck.isEmpty)) {
      players.find(_.deck.nonEmpty).get
    } else {
      val cards = players.map(p => p.number -> p.nextCard).toMap
      val winner =
        if (players.forall(p => p.deck.size > cards(p.number)))
          playSubGames(players.map(_.subGame)).number
        else
          cards.maxBy(_._2)._1
      playSubGames(players.map(p => if (p.number == winner) p.win(cards) else p.loose), playersStates + players)
    }

  private def getScore(player: Player) = player.deck.reverse.zipWithIndex.map {
    case (card, index) => card * (index + 1)
  }.sum

  private case class Player(number: Int, deck: Vector[Int]) {
    def nextCard: Int = deck.head
    def win(cards: Map[Int, Int]): Player = copy(deck = deck.tail :+ cards(number) :+ cards.find(_._1 != number).get._2)
    def loose: Player = copy(deck = deck.tail)
    def subGame: Player = copy(deck = deck.tail.take(deck.head))
  }
}

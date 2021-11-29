package adventofcode.y2020

import scala.annotation.tailrec

object Day23 extends Year2020 {
  override val day = 23

  override def runDay(input: String): Unit = {
    val allCups = input.map(_.asDigit).toList
    val (firstCup, highestCup) = (allCups.head, allCups.max)

    val nextCupsP1 = createCupsLoop(allCups)
    playGame(firstCup, nextCupsP1, 100, highestCup)
    val answerP1 = Iterator.iterate(nextCupsP1(1))(nextCupsP1(_)).takeWhile(_ != 1).mkString
    printDayPart(1, answerP1, "labels on the cups after cup 1: %s")

    val nextCupsP2 = createCupsLoop(allCups ::: (highestCup + 1 to 1000000).toList)
    playGame(firstCup, nextCupsP2, 10000000, 1000000)
    printDayPart(2, Iterator.iterate(nextCupsP2(1))(nextCupsP2(_)).take(2).map(_.toLong).product)
  }

  private def createCupsLoop(cups: Seq[Int]) = {
    val nextCups = new Array[Int](cups.size + 1)
    for ((cup, next) <- cups.view.zip(cups.view.drop(1) ++ cups.view.take(1))) nextCups(cup) = next
    nextCups
  }

  private def playGame(firstCup: Int, nextCups: Array[Int], rounds: Int, highestCup: Int) =
    (1 to rounds).foldLeft(firstCup)((currentCup, _) => playRound(nextCups, currentCup, highestCup))

  private def playRound(nextCups: Array[Int], current: Int, highestCup: Int): Int = {
    val r1 = nextCups(current)
    val r2 = nextCups(r1)
    val r3 = nextCups(r2)
    nextCups(current) = nextCups(r3) // remove 3 cups from loop

    @tailrec
    def findDestination(c: Int): Int =
      if (c == r1 || c == r2 || c == r3) findDestination(nextDestination(c)) else c
    def nextDestination(c: Int) = if (c > 1) c - 1 else highestCup

    val destination = findDestination(nextDestination(current))
    // insert 3 removed cups after destination
    nextCups(r3) = nextCups(destination)
    nextCups(destination) = r1

    nextCups(current)
  }
}

package adventofcode.y2022

import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day23 extends Year2022 {
  override val day = 23

  private val POSSIBLE_MOVES: List[List[Pos => Pos]] = List(
    List(_.up, _.up.right, _.up.left),
    List(_.down, _.down.right, _.down.left),
    List(_.left, _.left.up, _.left.down),
    List(_.right, _.right.up, _.right.down)
  )

  override def runDay(input: BufferedSource): Unit = {
    val elves = Pos.parseSet(input.getLines())

    val round10 = (0 until 10).foldLeft(elves) { case (elves, round) => simulateRound(elves, round) }
    val emptyAfter10 = (round10.minBy(_.y).y to round10.maxBy(_.y).y).map { y =>
      (round10.minBy(_.x).x to round10.maxBy(_.x).x).count(x => !round10.contains(Pos(x, y)))
    }.sum

    printDayPart(1, emptyAfter10, "Empty tiles after 10 rounds: %s")
    printDayPart(2, findEnd(round10, 10), "First round with no elf moving: %s")
  }

  @tailrec
  private def findEnd(elves: Set[Pos], round: Int): Int = {
    val next = simulateRound(elves, round)
    if (next == elves)
      round + 1
    else
      findEnd(next, round + 1)
  }

  private def simulateRound(elves: Set[Pos], round: Int): Set[Pos] = {
    val possibleTargets = elves.par.map(elf => elf -> getPossibleTarget(elf, elves, round)).collect {
      case (elf, Some(target)) => elf -> target
    }.toMap
    val targets = possibleTargets.values.groupBy(identity).mapValues(_.size)
    val elvesWhoCanMove = possibleTargets.filter(elf => targets(elf._2) == 1)
    elves -- elvesWhoCanMove.keySet ++ elvesWhoCanMove.values
  }

  private def getPossibleTarget(elf: Pos, elves: Set[Pos], round: Int): Option[Pos] =
    if (elf.neighbors.forall(!elves.contains(_)))
      None
    else
      (0 until 4).iterator.map(i => POSSIBLE_MOVES((round + i) % 4).map(_(elf))).collectFirst {
        case direction if direction.forall(!elves.contains(_)) => direction.head
      }
}

package adventofcode.y2020

import adventofcode.common.collections.{DoubleLinkedListWithIndex, Node}

import scala.annotation.tailrec

object Day23 extends Year2020 {
  override val day = 23

  private val allCups = inputString.map(_.asDigit).toList
  private val (lowestCup, highestCup) = (allCups.min, allCups.max)

  private val oneP1 = playGame(allCups.head, createLinkedList(allCups), 100, highestCup)
  private val answerP1 = Iterator.iterate(oneP1.next)(_.next).takeWhile(_.value != 1).map(_.value).mkString
  printDayPart(1, answerP1, "labels on the cups after cup 1: %s")

  private val oneP2 = playGame(allCups.head, createLinkedList(allCups ::: (highestCup + 1 to 1000000).toList), 10000000, 1000000)

  printDayPart(2, Iterator.iterate(oneP2.next)(_.next).take(2).map(_.value.toLong).product)

  private def createLinkedList(cups: List[Int]): DoubleLinkedListWithIndex[Int] = {
    val linkedCups = new DoubleLinkedListWithIndex(cups.head)
    cups.tail.foldLeft(linkedCups.get(cups.head)) { (prev, cup) =>
      linkedCups.insertAfter(cup, prev)
    }
    linkedCups
  }

  private def playGame(firstCup: Int, cups: DoubleLinkedListWithIndex[Int], rounds: Int, highestCup: Int): Node[Int] = {
    (1 to rounds).foldLeft(cups.get(firstCup)) { (currentCup, _) =>
      val removed = (1 to 3).foldLeft(currentCup, Vector.empty[Int]) {
        case ((cup, removed), _) =>
          val toRemove = cup.next
          cups.remove(toRemove.value)
          (toRemove, removed :+ toRemove.value)
      }._2
      val destination = findDestination(cups, currentCup.value - 1, highestCup: Int)
      removed.foldLeft(destination)((position, value) => cups.insertAfter(value, position))
      currentCup.next
    }
    cups.get(1)
  }

  @tailrec
  private def findDestination(cups: DoubleLinkedListWithIndex[Int], destination: Int, highestCup: Int): Node[Int] =
    if (cups.contains(destination))
      cups.get(destination)
    else if (destination <= lowestCup)
      findDestination(cups, highestCup, highestCup)
    else
      findDestination(cups, destination - 1, highestCup)
}

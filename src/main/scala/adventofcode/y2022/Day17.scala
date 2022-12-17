package adventofcode.y2022

import adventofcode.common.pos.Pos

import scala.annotation.tailrec

object Day17 extends Year2022 {
  override val day = 17

  private val ROCKS =
    """####
      |
      |.#.
      |###
      |.#.
      |
      |..#
      |..#
      |###
      |
      |#
      |#
      |#
      |#
      |
      |##
      |##""".stripMargin

  override def runDay(input: String): Unit = {
    val directions = input.toCharArray

    val rocks = ROCKS.split("\n\n").map(l => Pos.parseSet(l.split("\n").iterator))
      .map(r => r.map(_
        .moveDirectionIndex(0, r.maxBy(_.y).y - r.minBy(_.y).y)
        .moveDirectionIndex(1, 2))
      ).toList

    val loop = findLoop(rocks, directions)

    printDayPart(1, getFinalHeight(rocks, directions, loop), "Height after 2022 rocks: %s")
    printDayPart(2, getFinalHeight(rocks, directions, loop, 1000000000000L), "Height after 1000000000000 rocks: %s")
  }

  private def findLoop(rocks: List[Set[Pos]], directions: Array[Char]): Loop = {
    @tailrec
    def fall(chamber: Set[Pos], rocksOffset: Int = 0, directionsOffset: Int = 0, seen: List[(Int, Int, List[Int])] = List.empty, heights: List[Int] = List(0)): Loop = {
      val topLine = getTopLine(chamber)
      val currentState = (rocksOffset, directionsOffset, topLine)
      if (seen.contains(currentState)) {
        val loopSize = seen.indexOf(currentState) + 1
        val endHeight = currentHeight(chamber)

        val topLineOffset = topLine.min
        val loopEndFloor = topLine.zipWithIndex.map {
          case (height, x) => Pos(x, height - topLineOffset + 1)
        }.toSet
        val afterLoop = (loopEndFloor, rocksOffset, directionsOffset)

        Loop(loopSize, endHeight - heights(loopSize), seen.size, endHeight, afterLoop)
      } else {
        val (newChamber, newOffset) = fallRock(rocks(rocksOffset), chamber, directions, directionsOffset)
        fall(newChamber, (rocksOffset + 1) % rocks.size, newOffset, currentState :: seen, currentHeight(newChamber) :: heights)
      }
    }

    val floor = (0 to 6).map(Pos(_, 1)).toSet
    fall(floor)
  }

  private def getFinalHeight(rocks: List[Set[Pos]], directions: Array[Char], loop: Loop, finalRock: Long = 2022): Long = {
    val remainingRocks = finalRock - loop.endRocks

    val endChamber = (1 to (remainingRocks % loop.loopSize).toInt).foldLeft(loop.afterLoopState) {
      case ((chamber, rocksOffset, dirOffset), _) =>
        val (newChamber, newOffset) = fallRock(rocks(rocksOffset), chamber, directions, dirOffset)
        (newChamber, (rocksOffset + 1) % rocks.size, newOffset)
    }._1

    loop.endHeight + (remainingRocks / loop.loopSize * loop.loopHeight) + currentHeight(endChamber)
  }

  private def fallRock(rock: Set[Pos], chamber: Set[Pos], directions: Array[Char], directionsOffset: Int): (Set[Pos], Int) = {
    @tailrec
    def fall(rock: Set[Pos], directionsOffset: Int): (Set[Pos], Int) = {
      val movedRock = rock.map(_.move(directions(directionsOffset)))
      val blockedRock = if movedRock.exists(r => r.x < 0 || r.x > 6 || chamber.contains(r)) then rock else movedRock
      val fallenRock = blockedRock.map(_.down)

      if (fallenRock.exists(chamber.contains)) {
        (chamber ++ blockedRock, (directionsOffset + 1) % directions.length)
      } else {
        fall(fallenRock, (directionsOffset + 1) % directions.length)
      }
    }

    fall(rock.map(_.moveDirectionIndex(0, currentHeight(chamber) + 3)), directionsOffset)
  }

  private def currentHeight(chamber: Set[Pos]): Int = (chamber.minBy(_.y).y - 1) * -1

  private def getTopLine(chamber: Set[Pos]): List[Int] = {
    val topLine = chamber.groupBy(_.x).values.map(_.minBy(_.y)).toList.sortBy(_.x).map(_.y)
    val offset = topLine.max
    topLine.map(_ - offset)
  }

  private case class Loop(loopSize: Int, loopHeight: Int,
                          endRocks: Int, endHeight: Int,
                          afterLoopState: (Set[Pos], Int, Int))
}

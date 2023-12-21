package adventofcode.y2023

import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day21 extends Year2023 {
  override val day = 21

  private val TOTAL_STEPS = 26501365L

  override def runDay(input: BufferedSource): Unit = {
    val initMap = Pos.parseMap(input.getLines(), identity)
    val open = initMap.filter(_._2 == '.').keySet
    val start = initMap.find(_._2 == 'S').get._1
    val size = initMap.keys.max.x + 1

    val stepsNeeded = getStepsNeeded(open, start)
    val reachableEven = stepsNeeded.filter(_._2 % 2 == 0)
    val reachableOdd = stepsNeeded.filter(_._2 % 2 == 1)

    val reachedAfter64 = reachableEven.filter(p => p._2 <= 64)
    // Pos.printMapArea(Pos.zero, max, p => if (reachedAfter64.contains(p)) 'O' else initMap(p))

    printDayPart(1, reachedAfter64.size, "Number of reached garden plots after 64 steps: %s")

    val innerReachableEven = reachableEven.count(_._2 <= size / 2)
    val innerReachableOdd = reachableOdd.count(_._2 <= size / 2)

    val outerReachableEven = reachableEven.size - innerReachableEven
    val outerReachableOdd = reachableOdd.size - innerReachableOdd
    val allOuterReachable = outerReachableEven + outerReachableOdd

    val evenSquareSize = TOTAL_STEPS / size
    val oddSquareSize = evenSquareSize + 1

    val total = math.pow(oddSquareSize.toDouble, 2) * innerReachableOdd
      + math.pow(evenSquareSize.toDouble, 2) * innerReachableEven
      + evenSquareSize * oddSquareSize * allOuterReachable

    printDayPart(2, total.toLong, "Number of reached garden plots after 26501365 steps: %s")
  }

  private def getStepsNeeded(open: Set[Pos], start: Pos): Map[Pos, Long] = {
    @tailrec
    def inner(open: Set[Pos], current: Set[Pos], steps: Long, seen: Map[Pos, Long]): Map[Pos, Long] = {
      val next = current.flatMap(_.directions.filter(open.contains))
      val newSeen = seen ++ next.map(_ -> steps)
      if (next.isEmpty)
        newSeen
      else
        inner(open -- next, next, steps + 1, newSeen)
    }

    inner(open, Set(start), 1, Map(start -> 0))
  }
}

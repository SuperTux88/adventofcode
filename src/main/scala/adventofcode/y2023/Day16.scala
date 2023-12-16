package adventofcode.y2023

import adventofcode.Logging
import adventofcode.common.pos.{Direction, Pos}

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day16 extends Year2023 {
  override val day = 16

  override def runDay(input: BufferedSource): Unit = {
    val map = Pos.parseMap(input.getLines(), identity)
    val size = map.keys.max

    val energized = simulate(map, (Pos.zero, Direction.rightIndex))
    if (Logging.debug) Pos.printMapArea(Pos.zero, size, p => if (energized.contains(p)) '#' else '.')

    printDayPart(1, energized.size, "Number of energized tiles: %s")

    val edge = (0 to size.x).map(x => (Pos(x, 0), Direction.downIndex)) ++
      (0 to size.x).map(x => (Pos(x, size.y), Direction.upIndex)) ++
      (0 to size.y).map(y => (Pos(0, y), Direction.rightIndex)) ++
      (0 to size.y).map(y => (Pos(size.x, y), Direction.leftIndex))

    val edgeEnergized = edge.par.map(simulate(map, _).size)
    printDayPart(2, edgeEnergized.max, "Maximum number of energized tiles: %s")
  }

  private def simulate(map: Map[Pos, Char], start: (Pos, Int)) = {
    @tailrec
    def loop(current: List[(Pos, Int)], seen: Set[(Pos, Int)] = Set.empty): Set[Pos] =
      current match {
        case Nil => seen.map(_._1)
        case head :: tail if seen.contains(head) => loop(tail, seen)
        case (pos, dir) :: tail =>
          (map.get(pos), dir) match {
            case (None, _) =>
              loop(tail, seen)
            case (Some('/'), Direction.upIndex) | (Some('/'), Direction.downIndex) |
                 (Some('\\'), Direction.rightIndex) | (Some('\\'), Direction.leftIndex) =>
              loop(move(pos, Direction.rotateRight(dir)) :: tail, seen + ((pos, dir)))
            case (Some('/'), Direction.rightIndex) | (Some('/'), Direction.leftIndex) |
                 (Some('\\'), Direction.upIndex) | (Some('\\'), Direction.downIndex) =>
              loop(move(pos, Direction.rotateLeft(dir)) :: tail, seen + ((pos, dir)))
            case (Some('-'), Direction.upIndex) | (Some('-'), Direction.downIndex) |
                 (Some('|'), Direction.leftIndex) | (Some('|'), Direction.rightIndex) =>
              loop(move(pos, Direction.rotateRight(dir)) :: move(pos, Direction.rotateLeft(dir)) :: tail, seen + ((pos, dir)))
            case _ =>
              loop((pos.moveDirectionIndex(dir), dir) :: tail, seen + ((pos, dir)))
          }
      }

    def move(pos: Pos, dir: Int) = (pos.moveDirectionIndex(dir), dir)

    loop(List(start))
  }
}

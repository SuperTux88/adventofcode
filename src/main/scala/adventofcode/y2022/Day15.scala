package adventofcode.y2022

import adventofcode.common.NumberHelper.isInRange
import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day15 extends Year2022 {
  override val day = 15

  private val SensorRE = """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r

  private val ROW_P1 = 2000000
  private val RANGE = 4000000

  override def runDay(input: BufferedSource): Unit = {
    val sensors = input.getLines().map {
      case SensorRE(x, y, bx, by) => Sensor(Pos(x.toInt, y.toInt), Pos(bx.toInt, by.toInt))
    }.toList

    // this is optimized by the knowledge of part 2 that there can't be any gaps on this line
    // (unless part 2 would be on this exact line)
    val (rowStart, rowEnd) = sensors.flatMap(_.borderOnRow(ROW_P1)).foldLeft(Int.MaxValue, Int.MinValue) {
      case ((min, max), (start, end)) => (min.min(start), max.max(end))
    }
    val rowCount = rowEnd - rowStart + 1
    val beaconsOnRow = sensors.filter(_.beacon.y == ROW_P1).map(_.beacon.x).toSet

    printDayPart(1, rowCount - beaconsOnRow.size, "Positions which cannot contain a beacon: %s")

    val distressBeacon = sensors.combinations(2).flatMap {
      case List(a, b) if a.pos.distance(b.pos) == a.dist + b.dist + 2 =>
        a.outerBorder(a.pos.direction(b.pos))
          .filter(inRange).find(p => !sensors.exists(_.isInReach(p)))
      case _ => None
    }.next

    printDayPart(2, distressBeacon.x.toLong * 4000000 + distressBeacon.y.toLong, "Tuning frequency: %s")
  }

  private def inRange(p: Pos) = isInRange(p.x, 0, RANGE) && isInRange(p.y, 0, RANGE)

  private case class Sensor(pos: Pos, beacon: Pos) {
    val dist: Int = pos.distance(beacon)

    def isInReach(other: Pos): Boolean = pos.distance(other) <= dist

    def borderOnRow(row: Int): Option[(Int, Int)] = {
      val horizontalDiff = dist - (pos.y - row).abs
      if horizontalDiff >= 0 then Some((pos.x - horizontalDiff, pos.x + horizontalDiff)) else None
    }

    def outerBorder(direction: (Int, Int)): Iterator[Pos] =
      direction match {
        case (1, -1) => Pos(pos.x, pos.y - dist - 1).lineTo(Pos(pos.x + dist + 1, pos.y))
        case (-1, -1) => Pos(pos.x, pos.y - dist - 1).lineTo(Pos(pos.x - dist - 1, pos.y))
        case (1, 1) => Pos(pos.x, pos.y + dist + 1).lineTo(Pos(pos.x + dist + 1, pos.y))
        case (-1, 1) => Pos(pos.x, pos.y + dist + 1).lineTo(Pos(pos.x - dist - 1, pos.y))
      }
  }
}

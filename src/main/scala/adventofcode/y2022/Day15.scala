package adventofcode.y2022

import adventofcode.common.NumberHelper.isInRange
import adventofcode.common.pos.Pos

import scala.io.BufferedSource

object Day15 extends Year2022 {
  override val day = 15

  private val SensorRE = """Sensor at x=([\d-]+), y=([\d-]+): closest beacon is at x=([\d-]+), y=([\d-]+)""".r

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

    val distressBeacon = sensors.flatMap { sensor =>
      sensor.outerBorder.filter(inRange).find(p => !sensors.exists(_.isInReach(p)))
    }.head

    printDayPart(2, distressBeacon.x.toLong * 4000000 + distressBeacon.y.toLong, "Tuning frequency: %s")
  }

  private def inRange(p: Pos) = isInRange(p.x, 0, RANGE) && isInRange(p.y, 0, RANGE)

  private case class Sensor(pos: Pos, beacon: Pos) {
    private val dist: Int = pos.distance(beacon)

    def isInReach(other: Pos): Boolean = pos.distance(other) <= dist

    def borderOnRow(row: Int): Option[(Int, Int)] = {
      val horizontalDiff = dist - (pos.y - row).abs
      if horizontalDiff >= 0 then Some((pos.x - horizontalDiff, pos.x + horizontalDiff)) else None
    }

    def outerBorder: Iterator[Pos] = {
      val borderDist = dist + 1

      val topRight = Pos(pos.x, pos.y - borderDist).lineTo(Pos(pos.x + borderDist, pos.y))
      val topLeft = Pos(pos.x, pos.y - borderDist).lineTo(Pos(pos.x - borderDist, pos.y))
      val bottomRight = Pos(pos.x, pos.y + borderDist).lineTo(Pos(pos.x + borderDist, pos.y))
      val bottomLeft = Pos(pos.x, pos.y + borderDist).lineTo(Pos(pos.x - borderDist, pos.y))

      topRight ++ bottomRight ++ bottomLeft ++ topLeft
    }
  }
}

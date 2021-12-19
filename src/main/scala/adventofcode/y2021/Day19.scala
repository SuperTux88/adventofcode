package adventofcode.y2021

import adventofcode.common.IterableImplicits
import adventofcode.common.pos.Direction.DirectionPos3D
import adventofcode.common.pos.Pos3D

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParSeq

object Day19 extends Year2021 {
  override val day = 19

  private val ScannerRE = """--- scanner (\d+) ---""".r
  private val Pos3dRE = """(-?\d+),(-?\d+),(-?\d+)""".r

  override def runDay(input: String): Unit = {
    val scanners = input.split("\n\n").map(scanner => {
      val scannerLines = scanner.linesIterator
      val number = scannerLines.next match {
        case ScannerRE(number) => number.toInt
      }
      val beacons = scannerLines.map {
        case Pos3dRE(x, y, z) => Pos3D(x.toInt, y.toInt, z.toInt)
      }.toList
      Scanner(number, beacons)
    }).toSeq

    val (beacons, offsets) = findBeacons(scanners.tail.par, scanners.head.beacons.toSet)
    printDayPart(1, beacons.size, "number of beacons: %s")

    val distances = offsets.combinations(2).collect { case List(a, b) => (a, b) }.map(_.distance(_))
    printDayPart(2, distances.max, "largest distance between 2 scanners: %s")
  }

  @tailrec
  private def findBeacons(scanners: ParSeq[Scanner], beacons: Set[Pos3D], offsets: List[Pos3D] = List(Pos3D(0, 0, 0))): (Set[Pos3D], List[Pos3D]) = {
    val overlapping = scanners.flatMap(scanner => findRotation(scanner.rotate(), beacons))
    val (remainingScanners, newBeacons, newOffsets) = overlapping.foldLeft(scanners, beacons, offsets) {
      case ((scanners, beacons, offsets), (newScanner, offset)) =>
        val newBeacons = newScanner.beacons.map(_ + offset)
        (scanners.filterNot(_.number == newScanner.number), beacons ++ newBeacons.toSet, offset :: offsets)
    }
    if (remainingScanners.isEmpty)
      (newBeacons, newOffsets)
    else
      findBeacons(remainingScanners, newBeacons, newOffsets)
  }

  @tailrec
  private def findRotation(scanners: List[Scanner], beacons: Set[Pos3D]): Option[(Scanner, Pos3D)] =
    scanners match {
      case scanner :: rest =>
        scanner.getOverlapsOffset(beacons) match {
          case Some(offset) => Some(scanner, offset)
          case None => findRotation(rest, beacons)
        }
      case Nil => None
    }

  private case class Scanner(number: Int, beacons: List[Pos3D]) {
    def rotate(): List[Scanner] = (1 until 24).foldLeft(List(this)) { (scanners, index) =>
      copy(beacons = beacons.map(_.rotate(index))) :: scanners
    }
    def getOverlapsOffset(other: Set[Pos3D]): Option[Pos3D] = {
      val offsets = for {
        p1 <- beacons
        p2 <- other
      } yield p2 - p1
      offsets.groupCount(identity).find(_._2 >= 12).map(_._1)
    }
  }
}

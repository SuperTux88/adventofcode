package adventofcode.y2020

import adventofcode.common.pos.{Pos, Pos3D, Pos4D, PosTrait}

import scala.collection.parallel.CollectionConverters._

object Day17 extends Year2020 {
  override val day = 17

  private val initialSlice = Pos.parseMap(input.getLines(), char => char == '#').filter(_._2).keySet

  private val (maxX, maxY) = (initialSlice.maxBy(_.x).x, initialSlice.maxBy(_.y).y)

  private val map3D = initialSlice.map(p => Pos3D(p.x, p.y, 0))
  printDayPart(1, runCycles(map3D, nextPositions3D).size, "active cubes after 6 cycles: %s")

  private val map4D = initialSlice.map(p => Pos4D(p.x, p.y, 0, 0))
  printDayPart(2, runCycles(map4D, nextPositions4D).size, "active cubes after 6 cycles with 4 dimensions: %s")

  private def runCycles[T <: PosTrait[T]](map: Set[T], nextPositions: Int => Seq[T], cyclesToRun: Int = 6): Set[T] =
    (1 to cyclesToRun).foldLeft(map) { (state, index) =>
      nextPositions(index).par.flatMap { pos =>
        (state.contains(pos), pos.neighbors.count(state.contains)) match {
          case (true, 2 | 3) => Some(pos)
          case (false, 3) => Some(pos)
          case _ => None
        }
      }.toSet.seq
    }

  private def nextPositions3D(extraSize: Int) =
    for {
      x <- -extraSize to maxX + extraSize
      y <- -extraSize to maxY + extraSize
      z <- -extraSize to extraSize
    } yield Pos3D(x, y, z)

  private def nextPositions4D(extraSize: Int) =
    for {
      pos3d <- nextPositions3D(extraSize)
      w <- -extraSize to extraSize
    } yield Pos4D(pos3d.x, pos3d.y, pos3d.z, w)
}

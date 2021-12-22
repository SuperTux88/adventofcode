package adventofcode.y2021

import adventofcode.common.pos.Pos3D

import scala.io.BufferedSource

object Day22 extends Year2021 {
  override val day = 22

  private val CubeRE = """(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val cubes = input.getLines().takeWhile(_.nonEmpty).map {
      case CubeRE(on, x1, x2, y1, y2, z1, z2) =>
        Cube(on == "on", Pos3D(x1.toInt, y1.toInt, z1.toInt), Pos3D(x2.toInt, y2.toInt, z2.toInt))
    }.toList

    printDayPart(1, run(cubes.filter(_.from.x.abs <= 50)).map(_.size).sum,
      "number of active cubes inside initialization procedure area: %s")

    printDayPart(2, run(cubes).map(_.size).sum, "number of active cubes: %s")
  }

  private def run(cubes: List[Cube]): List[Cube] =
    cubes.tail.foldLeft(List(cubes.head)) { (list, cube) =>
      val withCanceledOutOverlaps = list.flatMap(_.invertedIntersectionOption(cube)) ::: list
      if (cube.on)
        cube :: withCanceledOutOverlaps
      else
        withCanceledOutOverlaps
    }

  private case class Cube(on: Boolean, from: Pos3D, to: Pos3D) {
    def invertedIntersectionOption(other: Cube): Option[Cube] =
      if (intersects(other))
        Some(Cube(!on,
          Pos3D(from.x max other.from.x, from.y max other.from.y, from.z max other.from.z),
          Pos3D(to.x min other.to.x, to.y min other.to.y, to.z min other.to.z)
        ))
      else
        None

    private def intersects(other: Cube) =
      from.x <= other.to.x && to.x >= other.from.x
        && from.y <= other.to.y && to.y >= other.from.y
        && from.z <= other.to.z && to.z >= other.from.z

    def size: Long = sign * from.cubeSize(to)
    private def sign = if on then 1 else -1
  }
}

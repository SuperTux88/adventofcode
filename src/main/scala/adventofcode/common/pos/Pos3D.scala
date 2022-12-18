package adventofcode.common.pos

import scala.annotation.targetName

case class Pos3D(x: Int, y: Int, z: Int) extends PosTrait[Pos3D] {
  @targetName("add")
  def +(direction: (Int, Int, Int)): Pos3D = Pos3D(x + direction._1, y + direction._2, z + direction._3)
  @targetName("add")
  def +(direction: Pos3D): Pos3D = Pos3D(x + direction.x, y + direction.y, z + direction.z)

  @targetName("subtract")
  def -(other: Pos3D): Pos3D = Pos3D(x - other.x, y - other.y, z - other.z)

  // Manhattan distance
  override def distance(other: Pos3D): Int = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs

  def cubeSize(to: Pos3D, including: Boolean = true): Long = {
    val offset = if including then 1 else 0
    ((x - to.x).abs.toLong + offset) * ((y - to.y).abs.toLong + offset) * ((z - to.z).abs.toLong + offset)
  }

  def directions: Seq[Pos3D] = Direction.directions3d.map(this + _)

  override def neighbors: Seq[Pos3D] =
    for {
      x <- -1 to 1
      y <- -1 to 1
      z <- -1 to 1
      if x != 0 || y != 0 || z != 0
    } yield this + (x, y, z)

  override def toString = s"$x,$y,$z"
}

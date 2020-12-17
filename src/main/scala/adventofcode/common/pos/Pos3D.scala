package adventofcode.common.pos

case class Pos3D(x: Int, y: Int, z: Int) extends PosTrait[Pos3D] {
  def +(direction: (Int, Int, Int)): Pos3D = Pos3D(x + direction._1, y + direction._2, z + direction._3)

  // Manhattan distance
  override def distance(other: Pos3D): Int = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs

  override def neighbors: Seq[Pos3D] =
    for {
      x <- -1 to 1
      y <- -1 to 1
      z <- -1 to 1
      if x != 0 || y != 0 || z != 0
    } yield this + (x, y, z)
}

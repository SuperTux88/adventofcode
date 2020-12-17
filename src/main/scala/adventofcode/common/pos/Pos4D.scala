package adventofcode.common.pos

case class Pos4D(x: Int, y: Int, z: Int, w: Int) extends PosTrait[Pos4D] {
  def +(direction: (Int, Int, Int, Int)): Pos4D =
    Pos4D(x + direction._1, y + direction._2, z + direction._3, w + direction._4)

  // Manhattan distance
  override def distance(other: Pos4D): Int =
    (x - other.x).abs + (y - other.y).abs + (z - other.z).abs + (w - other.w).abs

  override def neighbors: Seq[Pos4D] =
    for {
      x <- -1 to 1
      y <- -1 to 1
      z <- -1 to 1
      w <- -1 to 1
      if x != 0 || y != 0 || z != 0 || w != 0
    } yield this + (x, y, z, w)
}

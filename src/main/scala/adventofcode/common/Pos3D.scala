package adventofcode.common

case class Pos3D(x: Int, y: Int, z: Int) {
  def +(direction: (Int, Int, Int)): Pos3D = Pos3D(x + direction._1, y + direction._2, z + direction._3)

  // Manhattan distance
  def distance(other: Pos3D): Int = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs
}

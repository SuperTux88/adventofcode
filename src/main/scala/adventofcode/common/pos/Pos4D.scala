package adventofcode.common.pos

case class Pos4D(d1: Int, d2: Int, d3: Int, d4: Int) {
  def +(direction: (Int, Int, Int, Int)): Pos4D =
    Pos4D(d1 + direction._1, d2 + direction._2, d3 + direction._3, d4 + direction._4)

  // Manhattan distance
  def distance(other: Pos4D): Int =
    (d1 - other.d1).abs + (d2 - other.d2).abs + (d3 - other.d3).abs + (d4 - other.d4).abs
}

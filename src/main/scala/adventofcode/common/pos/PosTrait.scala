package adventofcode.common.pos

trait PosTrait[T <: PosTrait[T]] {
  // Manhattan distance
  def distance(other: T): Int

  // all neighbors including diagonals
  def neighbors: Seq[T]
}

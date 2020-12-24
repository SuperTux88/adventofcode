package adventofcode.common.pos

case class PosHex(x: Int, y: Int) extends PosTrait[PosHex] {
  def northWest: PosHex = copy(x = x - 1, y - 1)
  def northEast: PosHex = copy(x = x + 1, y - 1)
  def east: PosHex = copy(x = x + 2)
  def southEast: PosHex = copy(x = x + 1, y + 1)
  def southWest: PosHex = copy(x = x - 1, y + 1)
  def west: PosHex = copy(x = x - 2)

  // Manhattan distance
  override def distance(other: PosHex): Int = ((x - other.x).abs + (y - other.y).abs) / 2

  override def neighbors = Seq(northWest, northEast, east, southEast, southWest, west)
}

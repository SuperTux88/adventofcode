package adventofcode.common


case class Pos(x: Int, y: Int) extends Ordered[Pos] {
  def +(direction: (Int, Int)): Pos = Pos(x + direction._1, y + direction._2)

  def -(other: Pos): Pos = Pos(x - other.x, y - other.y)

  // Manhattan distance
  def distance(other: Pos): Int = (x - other.x).abs + (y - other.y).abs

  def up: Pos = copy(y = y - 1)
  def right: Pos = copy(x = x + 1)
  def down: Pos = copy(y = y + 1)
  def left: Pos = copy(x = x - 1)

  def move(direction: Char): Pos = direction match {
    case 'U'|'^' => up
    case 'R'|'>' => right
    case 'D'|'v' => down
    case 'L'|'<' => left
  }

  // 0 -> up, 1 -> right, 2 -> down, 3 -> left
  def moveDirectionIndex(direction: Int): Pos =
    Pos(x + Pos.directions(direction)._1, y + Pos.directions(direction)._2)

  def positive: Boolean = x >= 0 && y >= 0

  override def toString = s"$x,$y"

  import scala.math.Ordered.orderingToOrdered
  override def compare(that: Pos): Int = (this.y, this.x) compare (that.y, that.x)
}

object Pos {
  // up, right, down, left
  val directions = List((0, -1), (1, 0), (0, 1), (-1, 0))

  val zero: Pos = Pos(0, 0)
}

package adventofcode.common.pos

case class Pos(x: Int, y: Int) extends Ordered[Pos] {
  def +(direction: (Int, Int)): Pos = Pos(x + direction._1, y + direction._2)
  def +(direction: Pos): Pos = Pos(x + direction.x, y + direction.y)

  def -(other: Pos): Pos = Pos(x - other.x, y - other.y)

  def *(mul: Int): Pos = Pos(x * mul, y * mul)

  def /(div: Int): Pos = Pos(x / div, y / div)

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

  // rotate around 0,0 - only supports 90 degree steps
  def rotate(stepsClockwise: Int): Pos =
    stepsClockwise % 4 match {
      case 0 => this
      case 1 => Pos(-y, x)
      case 2 => Pos(-x, -y)
      case 3 => Pos(y, -x)
    }

  def neighbors: List[Pos] = Pos.directions.map(this + _)
  def neighborsWithDiagonals: List[Pos] = Pos.directionsWithDiagonals.map(this + _)
  def diagonals: List[Pos] = Pos.diagonals.map(this + _)

  def positive: Boolean = x >= 0 && y >= 0

  override def toString = s"$x,$y"

  import scala.math.Ordered.orderingToOrdered
  override def compare(that: Pos): Int = (this.y, this.x) compare (that.y, that.x)
}

object Pos {
  // up, right, down, left
  val directions: List[(Int, Int)] = List((0, -1), (1, 0), (0, 1), (-1, 0))
  // top-left, top-right, down-right, down-left
  val diagonals: List[(Int, Int)] = List((-1, -1), (1, -1), (1, 1), (-1, 1))
  val directionsWithDiagonals: List[(Int, Int)] = directions ::: diagonals

  val zero: Pos = Pos(0, 0)
}

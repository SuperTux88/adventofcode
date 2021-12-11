package adventofcode.common.pos

case class Pos(x: Int, y: Int) extends PosTrait[Pos] with Ordered[Pos] {
  def +(direction: (Int, Int)): Pos = Pos(x + direction._1, y + direction._2)
  def +(direction: Pos): Pos = Pos(x + direction.x, y + direction.y)

  def -(other: Pos): Pos = Pos(x - other.x, y - other.y)

  def *(mul: Int): Pos = Pos(x * mul, y * mul)

  def /(div: Int): Pos = Pos(x / div, y / div)

  // Manhattan distance
  override def distance(other: Pos): Int = (x - other.x).abs + (y - other.y).abs

  def up: Pos = copy(y = y - 1)
  def right: Pos = copy(x = x + 1)
  def down: Pos = copy(y = y + 1)
  def left: Pos = copy(x = x - 1)

  def move(direction: Char): Pos = direction match {
    case 'N' | 'U' | '^' => up
    case 'E' | 'R' | '>' => right
    case 'S' | 'D' | 'v' => down
    case 'W' | 'L' | '<' => left
  }

  // 0 -> up, 1 -> right, 2 -> down, 3 -> left
  def moveDirectionIndex(direction: Int, steps: Int = 1): Pos =
    Pos(x + Direction.directions(direction)._1 * steps, y + Direction.directions(direction)._2 * steps)

  def directions: List[Pos] = Direction.directions.map(this + _)
  override def neighbors: List[Pos] = Direction.directionsWithDiagonals.map(this + _)
  def diagonals: List[Pos] = Direction.diagonals.map(this + _)

  def positive: Boolean = x >= 0 && y >= 0

  override def toString = s"$x,$y"

  import scala.math.Ordered.orderingToOrdered
  override def compare(that: Pos): Int = (this.y, this.x) compare(that.y, that.x)
}

object Pos {
  def apply(pos: (Int, Int)): Pos = Pos(pos._1, pos._2)

  def parseMap[V](lines: Iterator[String], value: Char => V): Map[Pos, V] =
    lines.zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.map {
        case (char, x) => Pos(x, y) -> value(char)
      }
    }.toMap

  def printMap[V](map: Map[Pos, V], mapValue: V => (Char | String), min: Option[Pos] = None, max: Option[Pos] = None): Unit = {
    (min.getOrElse(map.keys.minBy(_.y)).y to max.getOrElse(map.keys.maxBy(_.y)).y).foreach(y =>
      println((min.getOrElse(map.keys.minBy(_.x)).x to max.getOrElse(map.keys.maxBy(_.x)).x)
        .map(x => mapValue(map(Pos(x, y)))).mkString)
    )
  }

  val zero: Pos = Pos(0, 0)
}

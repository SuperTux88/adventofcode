package adventofcode.common.pos

import scala.annotation.targetName
import scala.util.matching.Regex

case class Pos(x: Int, y: Int) extends PosTrait[Pos] with Ordered[Pos] {
  @targetName("add")
  def +(direction: (Int, Int)): Pos = Pos(x + direction._1, y + direction._2)
  @targetName("add")
  def +(direction: Pos): Pos = Pos(x + direction.x, y + direction.y)

  @targetName("subtract")
  def -(other: Pos): Pos = Pos(x - other.x, y - other.y)

  @targetName("multiply")
  def *(mul: Int): Pos = Pos(x * mul, y * mul)

  @targetName("multiply")
  def *(mul: Pos): Pos = Pos(x * mul.x, y * mul.y)

  @targetName("divide")
  def /(div: Int): Pos = Pos(x / div, y / div)

  @targetName("modulo")
  def %(div: Int): Pos = Pos(x % div, y % div)

  // Manhattan distance
  override def distance(other: Pos): Int = (x - other.x).abs + (y - other.y).abs

  def up: Pos = copy(y = y - 1)
  def right: Pos = copy(x = x + 1)
  def down: Pos = copy(y = y + 1)
  def left: Pos = copy(x = x - 1)

  def nextLine: Pos = Pos(0, y + 1)

  def move(direction: Char): Pos = direction match {
    case 'N' | 'U' | '^' => up
    case 'E' | 'R' | '>' => right
    case 'S' | 'D' | 'v' => down
    case 'W' | 'L' | '<' => left
  }

  // 0 -> up, 1 -> right, 2 -> down, 3 -> left
  def moveDirectionIndex(direction: Int, steps: Int = 1): Pos =
    Pos(x + Direction.directions(direction)._1 * steps, y + Direction.directions(direction)._2 * steps)

  def direction(other: Pos): (Int, Int) = (other.x.compare(this.x), other.y.compare(this.y))

  def lineTo(other: Pos): Iterator[Pos] = {
    val dir = direction(other)
    val Pos(distX, distY) = this - other
    Iterator.iterate(this)(_ + dir).take((distX.abs max distY.abs) + 1)
  }

  /**
    * Returns the positions that are on the border of a rectangle with this as the top left corner of the free space
    *
    * @param size size of the empty space in the middle, the border is one bigger in each direction
    * @return
    */
  def border(size: Pos): Set[Pos] = {
    val Pos(width, height) = size
    (-1 to width).map(x => Pos(this.x + x, y - 1)) ++
      (-1 to width).map(x => Pos(this.x + x, y + height)) ++
      (0 until height).map(y => Pos(x - 1, this.y + y)) ++
      (0 until height).map(y => Pos(x + width, this.y + y))
  }.toSet

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

  private val PosRE: Regex = """(-?\d+),(-?\d+)""".r

  def parse(s: String): Pos = s match {
    case PosRE(x, y) => Pos(x.toInt, y.toInt)
    case _ => throw new IllegalArgumentException(s"Cannot parse $s as Pos")
  }

  def parseSet(lines: Iterator[String], charToSearch: Char = '#'): Set[Pos] = lines.zipWithIndex.flatMap {
    case (line, y) =>
      line.zipWithIndex.flatMap {
        case (c, x) if c == charToSearch => Some(Pos(x, y))
        case _ => None
      }
  }.toSet

  def parseMap[V](lines: Iterator[String], value: Char => V): Map[Pos, V] =
    lines.zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.map {
        case (char, x) => Pos(x, y) -> value(char)
      }
    }.toMap

  def printMapArea(min: Pos, max: Pos, mapPos: Pos => (Char | String)): Unit =
    (min.y to max.y).foreach(y => println((min.x to max.x).map(x => mapPos(Pos(x, y))).mkString))

  def printMap[V](map: Map[Pos, V], mapValue: V => (Char | String), min: Option[Pos] = None, max: Option[Pos] = None): Unit =
    (min.getOrElse(map.keys.minBy(_.y)).y to max.getOrElse(map.keys.maxBy(_.y)).y).foreach(y =>
      println((min.getOrElse(map.keys.minBy(_.x)).x to max.getOrElse(map.keys.maxBy(_.x)).x)
        .map(x => mapValue(map(Pos(x, y)))).mkString)
    )

  val zero: Pos = Pos(0, 0)
}

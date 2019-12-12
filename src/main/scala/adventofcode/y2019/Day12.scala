package adventofcode.y2019

import adventofcode.common.Pos3D

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

object Day12 extends Year2019 {
  override val day = 12

  private val MoonRE = """<x=(-?\d+), y=(-?\d+), z=(-?\d+)>""".r
  private val axisFunctions = Seq[Pos3D => Int](_.x, _.y, _.z)

  private val moons = input.getLines().map {
    case MoonRE(x, y, z) => (Pos3D(x.toInt, y.toInt, z.toInt), (0, 0, 0))
  }.toVector

  val energy = Iterator.iterate(moons)(step).drop(1000).next.map {
    case (pos, vel) =>
      (math.abs(pos.x) + math.abs(pos.y) + math.abs(pos.z)) *
        (math.abs(vel._1) + math.abs(vel._2) + math.abs(vel._3))
  }.sum
  printDayPart(1, energy)

  val loops = axisFunctions.par.map(f => findLoopInAxis(step(moons), f, moons.map(moon => f(moon._1))))
  printDayPart(2, lcm(loops.seq.map(BigInt(_))).toLong)

  private def step(moons: Vector[(Pos3D, (Int, Int, Int))]) = {
    def compare(a: Pos3D, b: Pos3D, f: Pos3D => Int) = f(a).compareTo(f(b))

    (0 to 3).combinations(2).foldLeft(moons) { (moons, selected) =>
      val ((a, aVel), (b, bVel)) = (moons(selected.head), moons(selected.last))

      val diff = axisFunctions.map(compare(a, b, _))
      val aVel2 = (aVel._1 - diff.head, aVel._2 - diff(1), aVel._3 - diff(2))
      val bVel2 = (bVel._1 + diff.head, bVel._2 + diff(1), bVel._3 + diff(2))

      moons.updated(selected.head, (a, aVel2)).updated(selected.last, (b, bVel2))
    }.map {
      case (moon, vel) => (moon + vel, vel)
    }
  }

  @tailrec
  private def findLoopInAxis(moons: Vector[(Pos3D, (Int, Int, Int))], getAxis: Pos3D => Int, start: Vector[Int], count: Int = 1): Int = {
    val axis = moons.map(moon => getAxis(moon._1))
    if (start == axis) {
      count + 1
    } else {
      findLoopInAxis(step(moons), getAxis, start, count + 1)
    }
  }

  def lcm(numbers: Seq[BigInt]) = numbers.reduce((x: BigInt, y: BigInt) => x * (y / x.gcd(y)))
}

package adventofcode.y2019

import adventofcode.common.NumberHelper.lcm
import adventofcode.common.Pos3D

import scala.annotation.tailrec

object Day12 extends Year2019 {
  override val day = 12

  private val MoonRE = """<x=(-?\d+), y=(-?\d+), z=(-?\d+)>""".r
  private val axisFunctions = Seq[Pos3D => Int](_.x, _.y, _.z)
  private val velocityFunctions = Seq[((Int, Int, Int)) => Int](_._1, _._2, _._3)

  private val moons = input.getLines().takeWhile(_.nonEmpty).map {
    case MoonRE(x, y, z) => (Pos3D(x.toInt, y.toInt, z.toInt), (0, 0, 0))
  }.toVector

  private val energy = Iterator.iterate(moons)(step).drop(1000).next.map {
    case (pos, vel) =>
      (pos.x.abs + pos.y.abs + pos.z.abs) * (vel._1.abs + vel._2.abs + vel._3.abs)
  }.sum
  printDayPart(1, energy, "total energy after 1000 steps: %s")

  printDayPart(2, lcm(findLoop(step(moons)).map(_.toLong)), "loop after %s steps")

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
  private def findLoop(moons: Vector[(Pos3D, (Int, Int, Int))], loops: Seq[Int] = Seq(0, 0, 0), count: Int = 1): Seq[Int] = {
    if (!loops.contains(0)) {
      loops
    } else {
      val newLoops = velocityFunctions.lazyZip(loops).map {
        case (getVelocity, loop) =>
          if (Seq(0, 0, 0, 0) == moons.map(moon => getVelocity(moon._2)) && loop == 0) count * 2 else loop
      }
      findLoop(step(moons), newLoops, count + 1)
    }
  }
}

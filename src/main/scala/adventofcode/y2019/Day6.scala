package adventofcode.y2019

import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

object Day6 extends Year2019 {
  override val day = 6

  private val OrbitRE = """(\w+)\)(\w+)""".r

  override def runDay(input: BufferedSource): Unit = {
    val planets = input.getLines().takeWhile(_.nonEmpty).map {
      case OrbitRE(center, orbit) => orbit -> center
    }.toMap

    val planetOrbits = planets.par.map {
      case (orbit, center) => orbit -> Iterator.iterate(center)(planets).takeWhile(_ != "COM").toVector
    }

    printDayPart(1, planetOrbits.map(_._2.length + 1).sum, "number of direct and indirect orbits: %s")

    val youDist = planetOrbits("YOU").reverse.dropWhile(planetOrbits("SAN").contains(_)).length
    val santaDist = planetOrbits("SAN").length - (planetOrbits("YOU").length - youDist)

    printDayPart(2, youDist + santaDist, "distance to santa: %s")
  }
}

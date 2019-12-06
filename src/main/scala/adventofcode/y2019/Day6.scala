package adventofcode.y2019

import scala.collection.parallel.CollectionConverters._

object Day6 extends Year2019 {
  override val day = 6

  private val OrbitRE = """(\w+)\)(\w+)""".r

  private val planets = input.getLines().map {
    case OrbitRE(center, orbit) => orbit -> center
  }.toMap

  private val planetOrbits = planets.par.map {
    case (orbit, center) => orbit -> Iterator.iterate(center)(planets).takeWhile(_ != "COM").toVector
  }

  printDayPart(1, planetOrbits.map(_._2.length + 1).sum)

  private val youDist = planetOrbits("YOU").reverse.dropWhile(planetOrbits("SAN").contains(_)).length
  private val santaDist = planetOrbits("SAN").length - (planetOrbits("YOU").length - youDist)

  printDayPart(2, youDist + santaDist)
}

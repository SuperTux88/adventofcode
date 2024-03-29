package adventofcode.y2019

import scala.io.BufferedSource

object Day1 extends Year2019 {
  override val day = 1

  override def runDay(input: BufferedSource): Unit = {
    val masses = input.getLines().takeWhile(_.nonEmpty).map(_.toInt).toList

    printDayPart(1, masses.map(fuelNeeded).sum, "total fuel required: %s")

    val recursiveFuel = masses.map(mass => Iterator.iterate(fuelNeeded(mass))(fuelNeeded).takeWhile(_ > 0).sum).sum
    printDayPart(2, recursiveFuel, "total fuel required: %s")
  }

  private def fuelNeeded(mass: Int) = mass / 3 - 2
}

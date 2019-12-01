package adventofcode.y2019

object Day1 extends Year2019 {
  override val day = 1

  val masses = input.getLines().map(_.toInt).toList

  printDayPart(1, masses.map(fuelNeeded).sum)

  printDayPart(2, masses.map(mass => Iterator.iterate(fuelNeeded(mass))(fuelNeeded).takeWhile(_ > 0).sum).sum)

  private def fuelNeeded(mass: Int) = mass / 3 - 2
}

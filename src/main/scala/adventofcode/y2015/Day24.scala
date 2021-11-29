package adventofcode.y2015

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day24 extends Year2015 {
  override val day: Int = 24

  override def runDay(input: BufferedSource): Unit = {
    val packages = input.getLines().map(_.toInt).toList

    printDayPart(1, minQuantumEntanglementInCompartment(packages, 3).toString(), "quantum entanglement of group in compartment: %s")
    printDayPart(2, minQuantumEntanglementInCompartment(packages, 4).toString(), "quantum entanglement of group in compartment: %s")
  }

  private def minQuantumEntanglementInCompartment(packages: List[Int], groupSize: Int) = {
    val groupWeight = packages.sum / groupSize
    val minNumberOfPresents = minNumberOfPresentsInCompartment(packages, groupWeight)
    combinationsForCompartment(packages, groupWeight, minNumberOfPresents).map(_.map(BigInt(_)).product).min
  }

  private def minNumberOfPresentsInCompartment(packages: List[Int], groupWeight: Int) =
    (1 to packages.length).find(packages.takeRight(_).sum > groupWeight).get

  @tailrec
  private def combinationsForCompartment(packages: List[Int], groupWeight: Int, numberOfPresents: Int): Iterator[List[Int]] = {
    val combinations = packages.combinations(numberOfPresents).filter(_.sum == groupWeight)
    if (combinations.nonEmpty) combinations
    else combinationsForCompartment(packages, groupWeight, numberOfPresents + 1)
  }
}

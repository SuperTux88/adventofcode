package adventofcode.y2015

import scala.annotation.tailrec

object Day24 extends Year2015 {
  override val day: Int = 24

  val packages = input.getLines().map(_.toInt).toList

  printDayPart(1, minQuantumEntanglementInCompartment(3).toString(), "quantum entanglement of group in compartment: %s")
  printDayPart(2, minQuantumEntanglementInCompartment(4).toString(), "quantum entanglement of group in compartment: %s")

  private def minQuantumEntanglementInCompartment(groupSize: Int) = {
    val groupWeight = packages.sum / groupSize
    val minNumberOfPresents = minNumberOfPresentsInCompartment(groupWeight)
    combinationsForCompartment(groupWeight, minNumberOfPresents).map(_.map(BigInt(_)).product).min
  }

  private def minNumberOfPresentsInCompartment(groupWeight: Int) =
    (1 to packages.length).find(packages.takeRight(_).sum > groupWeight).get

  @tailrec
  private def combinationsForCompartment(groupWeight: Int, numberOfPresents: Int): Iterator[List[Int]] = {
    val combinations = packages.combinations(numberOfPresents).filter(_.sum == groupWeight)
    if (combinations.nonEmpty) combinations
    else combinationsForCompartment(groupWeight, numberOfPresents + 1)
  }
}

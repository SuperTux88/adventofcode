package adventofcode

import scala.annotation.tailrec

object Day24 extends DayApp {
  override val day: Int = 24

  val packages = input.getLines().map(_.toInt).toList

  printDayPart(1, s"quantum entanglement of group in compartment: ${minQuantumEntanglementInCompartment(3)}")
  printDayPart(2, s"quantum entanglement of group in compartment: ${minQuantumEntanglementInCompartment(4)}")

  def minQuantumEntanglementInCompartment(groupSize: Int) = {
    val groupWeight = packages.sum / groupSize
    val minNumberOfPresents = minNumberOfPresentsInCompartment(groupWeight)
    combinationsForCompartment(groupWeight, minNumberOfPresents).map(_.map(BigInt(_)).product).min
  }

  def minNumberOfPresentsInCompartment(groupWeight: Int) =
    (1 to packages.length).find(packages.takeRight(_).sum > groupWeight).get

  @tailrec
  def combinationsForCompartment(groupWeight: Int, numberOfPresents: Int): Iterator[List[Int]] = {
    val combinations = packages.combinations(numberOfPresents).filter(_.sum == groupWeight)
    if (combinations.nonEmpty) combinations
    else combinationsForCompartment(groupWeight, numberOfPresents + 1)
  }
}

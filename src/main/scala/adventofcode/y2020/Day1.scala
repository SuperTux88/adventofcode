package adventofcode.y2020

object Day1 extends Year2020 {
  override val day = 1

  private val numbers = input.getLines().map(_.toInt).toSeq

  printDayPart(1, productOfEntries(numbers, 2), "product of two entries: %s")
  printDayPart(2, productOfEntries(numbers, 3), "product of three entries: %s")

  private def productOfEntries(numbers: Seq[Int], n: Int) =
    numbers.combinations(n).find(_.sum == 2020).get.product
}

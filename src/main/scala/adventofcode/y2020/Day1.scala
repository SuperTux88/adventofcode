package adventofcode.y2020

object Day1 extends Year2020 {
  override val day = 1

  private val numbers = input.getLines().map(_.toInt).toSeq.sorted
  private val searchedSum = 2020

  printDayPart(1, productOfEntries(numbers, 2), "product of two entries: %s")
  printDayPart(2, productOfEntries(numbers, 3), "product of three entries: %s")

  private def productOfEntries(numbers: Seq[Int], n: Int) = {
    val found = numbers.combinations(n - 1).find(x => numbers.contains(searchedSum - x.sum)).get
    found.product * (searchedSum - found.sum)
  }
}

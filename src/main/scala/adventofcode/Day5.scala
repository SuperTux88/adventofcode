package adventofcode

object Day5 extends DayApp {
  override val day: Int = 5

  val vowels = "aeiou".toSet
  val badStringsRE = "(ab|cd|pq|xy)".r

  printDayPart(1, s"there are $part1 nice strings")
  printDayPart(2, s"there are $part2 nice strings")

  private def part1 = Input(5).lines.count(string => {
    badStringsRE.findFirstIn(string).isEmpty &&
      string.count(vowels) >= 3 &&
      string.sliding(2).exists { s => s(0) == s(1) }
  })

  private def part2 = Input(5).lines.count(string => {
    string.sliding(2).exists { s => string.indexOf(s) < string.lastIndexOf(s)-1} &&
      string.sliding(3).exists { s => s(0) == s(2) }
  })
}

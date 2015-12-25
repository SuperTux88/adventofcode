package adventofcode

object Day5 extends DayApp {
  override val day: Int = 5

  val vowels = "aeiou".toSet
  val badStringsRE = "(ab|cd|pq|xy)".r

  printDayPart(1, part1, "there are %s nice strings")
  printDayPart(2, part2, "there are %s nice strings")

  private def part1 = input.getLines().count(string => {
    badStringsRE.findFirstIn(string).isEmpty &&
      string.count(vowels) >= 3 &&
      string.sliding(2).exists { s => s(0) == s(1) }
  })

  private def part2 = input.getLines().count(string => {
    string.sliding(2).exists { s => string.indexOf(s) < string.lastIndexOf(s)-1} &&
      string.sliding(3).exists { s => s(0) == s(2) }
  })
}

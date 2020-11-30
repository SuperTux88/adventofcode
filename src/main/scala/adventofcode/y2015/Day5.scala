package adventofcode.y2015

object Day5 extends Year2015 {
  override val day: Int = 5

  val vowels = "aeiou".toSet
  val badStringsRE = "(ab|cd|pq|xy)".r

  printDayPart(1, part1, "there are %s nice strings")
  printDayPart(2, part2, "there are %s nice strings")

  private def part1 = input.getLines().count(string => {
    badStringsRE.findFirstIn(string).isEmpty &&
      string.count(vowels) >= 3 &&
      string.toSeq.sliding(2).map(_.unwrap).exists { s => s(0) == s(1) }
  })

  private def part2 = input.getLines().count(string => {
    string.toSeq.sliding(2).map(_.unwrap).exists { s => string.indexOf(s) < string.lastIndexOf(s)-1} &&
      string.toSeq.sliding(3).map(_.unwrap).exists { s => s(0) == s(2) }
  })
}

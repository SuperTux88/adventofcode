package adventofcode.y2015

import scala.io.BufferedSource

object Day5 extends Year2015 {
  override val day: Int = 5

  private val vowels = "aeiou".toSet
  private val badStringsRE = "(ab|cd|pq|xy)".r

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines().toSeq

    printDayPart(1, part1(lines), "there are %s nice strings")
    printDayPart(2, part2(lines), "there are %s nice strings")
  }

  private def part1(lines: Seq[String]) = lines.count(string => {
    badStringsRE.findFirstIn(string).isEmpty &&
      string.count(vowels) >= 3 &&
      string.toSeq.sliding(2).map(_.unwrap).exists { s => s(0) == s(1) }
  })

  private def part2(lines: Seq[String]) = lines.count(string => {
    string.toSeq.sliding(2).map(_.unwrap).exists { s => string.indexOf(s) < string.lastIndexOf(s)-1} &&
      string.toSeq.sliding(3).map(_.unwrap).exists { s => s(0) == s(2) }
  })
}

package adventofcode

object Day5 extends App {
  part1()
  part2()

  def part1() {
    val vowels = "aeiou".toSet
    val badStringsRE = "(ab|cd|pq|xy)".r

    val niceWords = Input(5).lines.count(string => {
      badStringsRE.findFirstIn(string).isEmpty &&
        string.count(vowels) >= 3 &&
        string.sliding(2).exists { s => s(0) == s(1) }
    })

    println(s"PART 1: there are $niceWords nice strings")
  }

  def part2(): Unit = {
    val niceWords = Input(5).lines.count(string => {
      string.sliding(2).exists { s => string.indexOf(s) < string.lastIndexOf(s)-1} &&
        string.sliding(3).exists { s => s(0) == s(2) }
    })

    println(s"PART 2: there are $niceWords nice strings")
  }
}

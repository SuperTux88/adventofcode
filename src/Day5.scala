
object Day5 extends App {
  part1()
  part2()

  def part1() {
    val vowels = List('a', 'e', 'i', 'o', 'u')
    val badStringsRE = "(ab|cd|pq|xy)".r

    val niceWords = Input(5).lines.count(string => {
      var lastChar = '-'

      badStringsRE.findFirstIn(string).isEmpty &&
        string.count(vowels.contains(_)) >= 3 &&
        string.exists( char =>
          if (lastChar == char) true
          else { lastChar = char; false }
        )
    })

    println(s"PART 1: there are $niceWords nice strings")
  }

  def part2(): Unit = {
    val niceWords = Input(5).lines.count(string => {
      var pairs: List[String] = List("--")

      var containsTwoLetterPairs, containsLetterPairWithOneLetterBetween = false

      string.foreach( char => {
        val pair = s"${pairs.head.tail}$char"

        if (pairs.head.head == char) containsLetterPairWithOneLetterBetween = true
        if (pairs.drop(1).indexOf(pair) >= 0) containsTwoLetterPairs = true

        pairs ::= pair
      })

      containsTwoLetterPairs && containsLetterPairWithOneLetterBetween
    })

    println(s"PART 2: there are $niceWords nice strings")
  }
}

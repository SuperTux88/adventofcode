package adventofcode.y2015

object Day11 extends Year2015 {
  override val day: Int = 11

  override def runDay(input: String): Unit = {
    val part1 = nextValidPassword(input)
    printDayPart(1, part1, "next valid password: %s")
    printDayPart(2, nextValidPassword(part1), "next valid password: %s")
  }

  private def nextValidPassword(oldPassword: String) = {
    var reversePassword = oldPassword.toList.reverse

    do {
      reversePassword = nextPassword(reversePassword)
    } while (!containsIncreasingStraightOfThreeChars(reversePassword) || !containsTwoPairs(reversePassword))

    reversePassword.reverse.mkString
  }

  private def nextPassword(reversePassword: List[Char]): List[Char] = reversePassword match {
    case 'z' :: rest => 'a' :: nextPassword(rest)
    case c :: rest if c == 'h' || c == 'k' || c == 'n' => (c + 2).toChar :: rest
    case c :: rest => (c + 1).toChar :: rest
    case Nil => List('a')
  }

  private def containsIncreasingStraightOfThreeChars(reversePassword: List[Char]) = {
    reversePassword.sliding(3).exists { x =>
      (x: @unchecked) match {
        case Seq(a, b, c) => a == b + 1 && a == c + 2
      }
    }
  }

  private def containsTwoPairs(reversePassword: List[Char]) = {
    val pairIndexes = reversePassword.sliding(2).zipWithIndex.collect {
      case (pair, index) if pair.head == pair.last => index
    }.toSeq
    pairIndexes.length >= 2 && pairIndexes.head < pairIndexes.last - 1
  }
}

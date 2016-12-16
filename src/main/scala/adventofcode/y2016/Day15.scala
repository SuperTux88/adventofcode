package adventofcode.y2016

object Day15 extends Year2016 {
  override val day = 15

  val DiscRE = """Disc #(\d) has (\d+) positions; at time=0, it is at position (\d+).""".r

  private val discs = input.getLines.map {
    case DiscRE(discNo, positions, currentPos) => Disc(discNo.toInt, positions.toInt, currentPos.toInt)
  }.toList

  printDayPart(1, chineseRemainderTheorem(discs))
  printDayPart(2, chineseRemainderTheorem(Disc(discs.length + 1, 11, 0) :: discs))

  private def chineseRemainderTheorem(discs: List[Disc]) = {
    val max = discs.map(_.positions).product
    val sum = discs.map { disc =>
      disc.remainder * max * multiplicativeInverse(max/disc.positions, disc.positions) / disc.positions
    }.sum

    sum % max
  }

  private def multiplicativeInverse(r: Int, m: Int) = {
    var x = (0, 1)
    var mod = m
    var remainder = r

    while (remainder != 0) {
      val quotient = mod / remainder
      x = (x._2, x._1 - quotient * x._2)

      val tmp = mod
      mod = remainder
      remainder = tmp % remainder
    }

    if (x._1 < 0) x._1 + m else x._1
  }

  private case class Disc(discNo: Int, positions: Int, startPos: Int) {
    def remainder: Int = (positions - startPos - discNo) % positions
  }
}

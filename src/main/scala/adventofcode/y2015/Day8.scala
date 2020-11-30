package adventofcode.y2015

object Day8 extends Year2015 {
  override val day: Int = 8

  val AsciiRE = """(\\x..|\\\"|\\\\)""".r
  val charsToEscape = Set('\\', '"')

  //printDayPart(1, part1)
  printDayPart(1, part1NoRegex)
  printDayPart(2, part2)

  private def part1 = input.getLines().map { line =>
    line.length - AsciiRE.replaceAllIn(line, " ").length + 2
  }.sum

  private def part1NoRegex = input.getLines().map(_.toCharArray).map { line =>
    var count = 0
    val it = line.iterator
    while(it.hasNext) {
      it.next() match {
        case '\\' => count += (it.next() match {
            case 'x' => it.next(); it.next(); 3
            case _ => 1
          })
        case '"' => count += 1
        case _ =>
      }
    }
    count
  }.sum

  private def part2 = input.getLines().map { line =>
    line.count(charsToEscape) + 2
  }.sum
}

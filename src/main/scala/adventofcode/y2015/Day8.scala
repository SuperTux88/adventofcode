package adventofcode.y2015

import scala.io.BufferedSource

object Day8 extends Year2015 {
  override val day: Int = 8

  private val AsciiRE = """(\\x..|\\"|\\\\)""".r
  private val charsToEscape = Set('\\', '"')

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines().toSeq

    //printDayPart(1, part1(lines))
    printDayPart(1, part1NoRegex(lines))
    printDayPart(2, part2(lines))
  }

  private def part1(lines: Seq[String]) = lines.map { line =>
    line.length - AsciiRE.replaceAllIn(line, " ").length + 2
  }.sum

  private def part1NoRegex(lines: Seq[String]) = lines.map(_.toCharArray).map { line =>
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

  private def part2(lines: Seq[String]) = lines.map { line =>
    line.count(charsToEscape) + 2
  }.sum
}

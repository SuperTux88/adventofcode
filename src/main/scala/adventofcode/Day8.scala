package adventofcode

object Day8 extends App {
  val AsciiRE = """(\\x..|\\\"|\\\\)""".r
  val charsToEscape = Set('\\', '"')

  val total1 = Input(8).lines.map { line =>
    line.length - AsciiRE.replaceAllIn(line, " ").length + 2
  }.sum

  val total1x = Input(8).lines.map(_.toCharArray).map { line =>
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

  val total2 = Input(8).lines.map { line =>
    line.count(charsToEscape) + 2
  }.sum

  println(s"PART 1: $total1")
  println(s"PART 1: $total1x")
  println(s"PART 2: $total2")
}

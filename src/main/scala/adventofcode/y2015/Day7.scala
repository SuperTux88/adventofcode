package adventofcode.y2015

import scala.io.BufferedSource

object Day7 extends Year2015 {
  override val day: Int = 7

  private val InstructionRE = """([\w ]+) -> (\w+)""".r
  private val SignalRE = """(\d+)""".r
  private val WireRE = """(\w+)""".r
  private val AndRE = """(\w+) AND (\w+)""".r
  private val OrRE = """(\w+) OR (\w+)""".r
  private val LShiftRE = """(\w+) LSHIFT (\d+)""".r
  private val RShiftRE = """(\w+) RSHIFT (\d+)""".r
  private val NotRE = """NOT (\w+)""".r

  override def runDay(input: BufferedSource): Unit = {
    var wires = input.getLines().map {
      case InstructionRE(inputs, output) => output -> inputs
    }.toMap

    var signals = Map.empty[String, Int]

    def wire(wireName: String): Int =
      signals.getOrElse(wireName, {
        import language.implicitConversions
        implicit def stringToInt(x: String): Int = augmentString(x).toInt

        wireName match {
          case SignalRE(in) => in
          case wireName: String =>
            val signal: Int = wires(wireName) match {
              case WireRE(in) => wire(in)
              case AndRE(in1, in2) => wire(in1) & wire(in2)
              case OrRE(in1, in2) => wire(in1) | wire(in2)
              case LShiftRE(in, positions) => wire(in) << positions
              case RShiftRE(in, positions) => wire(in) >> positions
              case NotRE(in) => ~wire(in)
            }
            signals += wireName -> signal
            signal
        }
      })

    val part1 = wire("a")

    wires += ("b" -> part1.toString)
    signals = Map.empty[String, Int]
    val part2 = wire("a")

    printDayPart(1, part1)
    printDayPart(2, part2)
  }
}

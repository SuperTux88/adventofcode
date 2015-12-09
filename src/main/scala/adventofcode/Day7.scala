package adventofcode

object Day7 extends DayApp {
  override val day: Int = 7

  val InstructionRE = """([\w ]+) -> (\w+)""".r
  val SignalRE = """(\d+)""".r
  val WireRE = """(\w+)""".r
  val AndRE = """(\w+) AND (\w+)""".r
  val OrRE = """(\w+) OR (\w+)""".r
  val LShiftRE = """(\w+) LSHIFT (\d+)""".r
  val RShiftRE = """(\w+) RSHIFT (\d+)""".r
  val NotRE = """NOT (\w+)""".r

  var wires = Input(7).lines.map {
    case InstructionRE(inputs, output) => output -> inputs
  }.toMap

  var signals = Map.empty[String, Int]

  private def input(wire: String): Int =
    signals.getOrElse(wire, {
      implicit def stringToInt(x: String): Int = augmentString(x).toInt

      wire match {
        case SignalRE(in) => in
        case wire: String => {
          val signal: Int = wires(wire) match {
            case WireRE(in) => input(in)
            case AndRE(in1, in2) => input(in1) & input(in2)
            case OrRE(in1, in2) => input(in1) | input(in2)
            case LShiftRE(in, positions) => input(in) << positions
            case RShiftRE(in, positions) => input(in) >> positions
            case NotRE(in) => ~input(in)
          }
          signals += wire -> signal
          signal
        }
      }
    })

  val part1 = input("a")

  wires += ("b" -> part1.toString)
  signals = Map.empty[String, Int]
  val part2 = input("a")

  printDayPart(1, part1)
  printDayPart(2, part2)
}

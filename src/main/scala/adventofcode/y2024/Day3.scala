package adventofcode.y2024

object Day3 extends Year2024 {
  override val day = 3

  private val MulRE = """mul\((\d+),(\d+)\)""".r
  private val DoRE = """do\(\)""".r
  private val DontRE = """don't\(\)""".r
  private val InstructionRe = s"""$MulRE|$DoRE|$DontRE""".r

  override def runDay(input: String): Unit = {
    val matches = InstructionRe.findAllIn(input)
    val instructions = matches.map {
      case MulRE(a, b) => Mul(a.toInt, b.toInt)
      case DoRE() => Do()
      case DontRE() => Dont()
    }.toList

    val result1 = instructions.collect { case Mul(a, b) => a * b }.sum
    printDayPart(1, result1, "Sum of multiplications: %s")

    val (result2, _) = instructions.foldLeft((0, true)) { case ((acc, state), instruction) =>
      instruction match {
        case Mul(a, b) if state => (acc + a * b, state)
        case Mul(a, b) => (acc, state)
        case Do() => (acc, true)
        case Dont() => (acc, false)
      }
    }
    printDayPart(2, result2, "Sum of just the enabled multiplications: %s")
  }

  private sealed trait Instruction
  private case class Mul(a: Int, b: Int) extends Instruction
  private case class Do() extends Instruction
  private case class Dont() extends Instruction
}

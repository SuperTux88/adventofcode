package adventofcode.y2016

object Day21 extends Year2016 {
  override val day = 21

  val SwapPositionRE = """swap position (\d) with position (\d)""".r
  val SwapLetterRE = """swap letter (\w) with letter (\w)""".r
  val RotateRE = """rotate (left|right) (\d) steps?""".r
  val RotatePositionRE = """rotate based on position of letter (\w)""".r
  val ReverseRE = """reverse positions (\d) through (\d)""".r
  val MoveRE = """move position (\d) to position (\d)""".r

  val instructions = input.getLines().toList

  printDayPart(1, scramblePassword(instructions, "abcdefgh"), "scrambled password: %s")
  printDayPart(2, scramblePassword(instructions.reverse, "fbgdceah", reverse = true), "un-scrambled password: %s")

  private def scramblePassword(instructions: List[String], password: String, reverse: Boolean = false) = {
    instructions.foldLeft(password) { (password, instruction) =>
      instruction match {
        case SwapPositionRE(a, b) => password.swapPosition(a.toInt, b.toInt)
        case SwapLetterRE(a, b) => password.swapLetter(a.head, b.head)
        case RotateRE(direction, steps) => (direction, reverse) match {
          case ("left", false)|("right", true) => password.rotateLeft(steps.toInt)
          case ("right", false)|("left", true) => password.rotateRight(steps.toInt)
          case dir => throw new MatchError(s"Invalid $dir")
        }
        case RotatePositionRE(letter) =>
          if (reverse)
            (1 to password.length).map(steps => password.rotateLeft(steps))
              .find(password == _.rotatePosition(letter)).get
          else
            password.rotatePosition(letter)
        case ReverseRE(from, to) => password.reverseSubstring(from.toInt, to.toInt)
        case MoveRE(from, to) =>
          if (reverse)
            password.move(to.toInt, from.toInt)
          else
            password.move(from.toInt, to.toInt)
      }
    }
  }

  private implicit class Password(password: String) {
    def swapPosition(a: Int, b: Int): String = password.updated(a, password(b)).updated(b, password(a))
    def swapLetter(a: Char, b: Char): String = password.map {
      case c if c == a => b
      case c if c == b => a
      case c => c
    }

    def rotateLeft(steps: Int): String = password.drop(steps) + password.take(steps)
    def rotateRight(steps: Int): String = password.takeRight(steps) + password.dropRight(steps)

    def rotatePosition(letter: String): String = {
      val position = password.indexOf(letter)
      val steps = (if (position >= 4) position + 2 else position + 1) % password.length
      rotateRight(steps)
    }

    def reverseSubstring(from: Int, to: Int): String =
      password.take(from) + password.substring(from, to + 1).reverse + password.drop(to + 1)

    def move(from: Int, to: Int): String = {
      val removed = password.take(from) + password.drop(from + 1)
      removed.take(to) + password(from) + removed.drop(to)
    }
  }
}

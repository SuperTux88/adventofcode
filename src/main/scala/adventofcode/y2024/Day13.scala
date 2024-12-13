package adventofcode.y2024

import adventofcode.common.pos.Pos

object Day13 extends Year2024 {
  override val day = 13

  private val ButtonRE = """Button ([AB]): X\+(\d+), Y\+(\d+)""".r
  private val PrizeRE = """Prize: X=(\d+), Y=(\d+)""".r

  override def runDay(input: String): Unit = {
    val machines = input.split("\n\n").map { machine =>
      val lines = machine.split("\n")
      val buttons = lines.takeWhile(_.startsWith("Button")).map {
        case ButtonRE(button, x, y) => Pos(x.toInt, y.toInt)
      }
      val prize = lines.collectFirst { case PrizeRE(x, y) => Pos(x.toInt, y.toInt) }.get
      Machine(buttons(0), buttons(1), prize)
    }.toSeq

    val buttons = machines.flatMap(_.findButtonPresses())
    printDayPart(1, getTokens(buttons), "Tokens needed to win all possible prizes: %s")

    val buttons2 = machines.flatMap(_.findButtonPresses(10000000000000L))
    printDayPart(2, getTokens(buttons2), "Tokens needed to win prizes with offset: %s")
  }

  private def getTokens(buttons: Seq[(Long, Long)]): Long =
    buttons.map { case (a, b) => a * 3 + b }.sum

  private case class Machine(buttonA: Pos, buttonB: Pos, prize: Pos) {
    // buttonA.x * a + buttonB.x * b = prize.x
    // buttonA.y * a + buttonB.y * b = prize.y
    def findButtonPresses(prizeOffset: Long = 0L): Option[(Long, Long)] = {
      val (x, y) = (prize.x + prizeOffset, prize.y + prizeOffset)
      val a = (x * buttonB.y - y * buttonB.x) / (buttonA.x * buttonB.y - buttonA.y * buttonB.x)
      val b = (x - buttonA.x * a) / buttonB.x
      if (buttonA.x * a + buttonB.x * b == x && buttonA.y * a + buttonB.y * b == y) Some((a, b))
      else None
    }
  }
}

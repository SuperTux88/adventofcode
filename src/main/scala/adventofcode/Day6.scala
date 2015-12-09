package adventofcode

object Day6 extends DayApp {
  override val day: Int = 6

  val ActionRE = """([a-z\s]+) (\d+),(\d+) through (\d+),(\d+)""".r

  val matrix = Array.fill(1000, 1000)((false, 0))

  Input(6).lines.foreach {
    case ActionRE(action, startX, startY, endX, endY) =>
      for (x <- startX.toInt to endX.toInt;
           y <- startY.toInt to endY.toInt)
        matrix(x)(y) = {
          val value = matrix(x)(y)
          action match {
            case "turn on" => (true, value._2 + 1)
            case "turn off" => (false, math.max(0, value._2 - 1))
            case "toggle" => (!value._1, value._2 + 2)
          }
        }
  }

  printDayPart(1, s"total ${matrix.map(_.count(_._1)).sum} lights are lit")
  printDayPart(2, s"total brightness: ${matrix.map(_.map(_._2).sum).sum}")
}

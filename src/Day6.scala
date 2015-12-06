
object Day6 extends App {
  val ActionRE = "([a-z\\s]+) (\\d+),(\\d+) through (\\d+),(\\d+)".r

  println(s"PART 1: total ${run(part1)} lights are lit")
  println(s"PART 2: total brightness: ${run(part2)}")

  private def run(callback: (String, Int) => Int): Int = {
    val matrix = Array.fill(1000, 1000)(0)

    Input(6).lines.foreach {
      case ActionRE(action, startX, startY, endX, endY) =>
        (startX.toInt to endX.toInt).foreach { x =>
          (startY.toInt to endY.toInt).foreach { y =>
            matrix(x)(y) = callback(action, matrix(x)(y))
          }
        }
    }

    matrix.map(_.sum).sum
  }

  private def part1(action: String, value: Int): Int =
    action match {
      case "turn on" => 1
      case "turn off" => 0
      case "toggle" => value match {
        case 1 => 0
        case 0 => 1
        }
    }


  private def part2(action: String, value: Int): Int =
    action match {
      case "turn on" => value + 1
      case "turn off" => value match {
        case 0 => 0
        case v => v - 1
      }
      case "toggle" => value + 2
    }
}

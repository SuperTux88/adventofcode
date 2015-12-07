
object Day6 extends App {
  val ActionRE = "([a-z\\s]+) (\\d+),(\\d+) through (\\d+),(\\d+)".r

  val matrix = Array.fill(1000, 1000)((false, 0))

  Input(6).lines.foreach {
    case ActionRE(action, startX, startY, endX, endY) =>
      (startX.toInt to endX.toInt).foreach { x =>
        (startY.toInt to endY.toInt).foreach { y =>
          matrix(x)(y) = {
            val value = matrix(x)(y)
            action match {
              case "turn on" => (true, value._2 + 1)
              case "turn off" => (false, value._2 match {
                case 0 => 0
                case v => v - 1
              })
              case "toggle" => (!value._1, value._2 + 2)
            }
          }
        }
      }
  }

  println(s"PART 1: total ${matrix.map(_.count(_._1)).sum} lights are lit")
  println(s"PART 2: total brightness: ${matrix.map(_.map(_._2).sum).sum}")
}

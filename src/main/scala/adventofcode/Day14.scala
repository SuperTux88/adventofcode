package adventofcode

object Day14 extends DayApp {
  override val day: Int = 14

  val ReindeerRE = """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

  val reindeer = input.getLines().map {
    case ReindeerRE(name, flySpeed, flySeconds, restSeconds) =>
      Reindeer(name, flySpeed.toInt, flySeconds.toInt, restSeconds.toInt)
  }.toList

  (1 to 2503).foreach { i =>
    reindeer.foreach(_.tick())
    reindeer.filter(_.distance == reindeer.map(_.distance).max).foreach(_.addPoint())
  }

  printDayPart(1, s"distance of the winner: ${reindeer.map(_.distance).max}")
  printDayPart(2, s"points of the winner: ${reindeer.map(_.points).max}")

  case class Reindeer(name: String, flySpeed: Int, flySeconds: Int, restSeconds: Int) {
    var distance = 0
    var points = 0
    var flying = true
    var nextStateChangeIn = flySeconds

    def tick() {
      if (flying) distance += flySpeed
      nextStateChangeIn -= 1
      if (nextStateChangeIn == 0) {
        flying = !flying
        nextStateChangeIn = if (flying) flySeconds else restSeconds
      }
    }

    def addPoint() = points += 1
  }
}

package adventofcode.y2015

object Day14 extends Year2015 {
  override val day: Int = 14

  val ReindeerRE = """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

  private val reindeer = input.getLines().map {
    case ReindeerRE(name, flySpeed, flySeconds, restSeconds) =>
      Reindeer(name, flySpeed.toInt, flySeconds.toInt, restSeconds.toInt)
  }.toList

  (1 to 2503).foreach { _ =>
    reindeer.foreach(_.tick())
    reindeer.filter(_.distance == reindeer.map(_.distance).max).foreach(_.addPoint())
  }

  printDayPart(1, reindeer.map(_.distance).max, "distance of the winner: %s")
  printDayPart(2, reindeer.map(_.points).max, "points of the winner: %s")

  private case class Reindeer(name: String, flySpeed: Int, flySeconds: Int, restSeconds: Int) {
    var distance = 0
    var points = 0
    var flying = true
    private var nextStateChangeIn = flySeconds

    def tick() {
      if (flying) distance += flySpeed
      nextStateChangeIn -= 1
      if (nextStateChangeIn == 0) {
        flying = !flying
        nextStateChangeIn = if (flying) flySeconds else restSeconds
      }
    }

    def addPoint() { points += 1 }
  }
}

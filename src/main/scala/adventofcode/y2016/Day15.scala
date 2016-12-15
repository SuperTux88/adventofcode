package adventofcode.y2016

object Day15 extends Year2016 {
  override val day = 15

  val DiscRE = """Disc #(\d) has (\d+) positions; at time=0, it is at position (\d+).""".r

  private val discs = input.getLines.map {
    case DiscRE(discNo, positions, currentPos) =>
      (1 to discNo.toInt).foldLeft(Disc(positions.toInt, currentPos.toInt))((disc, _) => disc.rotate)
  }.toList

  printDayPart(1, solve(discs))
  printDayPart(2, solve(Disc(11, discs.length + 1) :: discs))

  private def solve(discs: List[Disc]) = {
    var startTime = 0
    var currentDiscs = discs

    while(currentDiscs.exists(_.currentPos != 0)) {
      currentDiscs = currentDiscs.map(_.rotate)
      startTime += 1
    }

    startTime
  }

  private case class Disc(positions: Int, currentPos: Int) {
    def rotate: Disc = copy(currentPos = (currentPos + 1) % positions)
  }
}

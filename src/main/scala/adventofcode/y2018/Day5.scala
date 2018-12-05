package adventofcode.y2018

object Day5 extends Year2018 {
  override val day = 5

  def react(polymer: String) = {
    var currentPolymer = polymer
    var reduced = true

    while (reduced) {
      reduced = false
      for (c <- 'a' to 'z') {
        val nextPolymer = currentPolymer.replaceAll(s"$c${c.toUpper}", "").replaceAll(s"${c.toUpper}$c", "")
        if (nextPolymer != currentPolymer) reduced = true
        currentPolymer = nextPolymer
      }
    }
    currentPolymer
  }

  val polymerAfterPart1 = react(input.mkString)
  printDayPart(1, polymerAfterPart1.length)

  val shortest = ('a' to 'z').map { c => react(polymerAfterPart1.filterNot(_.toLower == c)).length }.min
  printDayPart(2, shortest)
}

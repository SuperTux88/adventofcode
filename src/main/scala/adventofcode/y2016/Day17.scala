package adventofcode.y2016

import java.security.MessageDigest

object Day17 extends Year2016 {
  override val day = 17

  override def runDay(input: String): Unit = {
    val paths = getPaths(State(0, 0, ""), input)

    printDayPart(1, paths._1, "shortest path: %s")
    printDayPart(2, paths._2.length, "longest path length: %s")
  }

  private def getPaths(startState: State, passcode: String): (String, String) = {
    var currentStates = List(startState)
    var shortestPath: String = ""
    var longestPath: String = ""

    while (currentStates.nonEmpty) {
      val (solutions, nextStates) = currentStates.flatMap(_.possibleMoves(passcode)).partition(_.isVault)
      solutions.headOption.foreach { solution =>
        longestPath = solution.path
        if (shortestPath.isEmpty) shortestPath = solution.path
      }
      currentStates = nextStates
    }

    (shortestPath, longestPath)
  }

  private case class State(x: Int, y: Int, path: String) {
    def possibleMoves(passcode: String): List[State] = {
      MessageDigest.getInstance("MD5").digest((passcode + path).getBytes)
        .take(2).map("%02x".format(_)).mkString.zipWithIndex.flatMap {
        case ('b'|'c'|'d'|'e'|'f', direction) => Some(this + State.directions(direction))
        case _ => None
      }.filter(_.isInGrid).toList
    }
    def isVault: Boolean = x == 3 && y == 3
    private def +(direction: (Int, Int, Char)) = State(x + direction._1, y + direction._2, path + direction._3)
    private def isInGrid = x >= 0 && y >= 0 && x < 4 && y < 4
  }
  private object State {
    val directions = List((0, -1, 'U'), (0, 1, 'D'), (-1, 0, 'L'), (1, 0, 'R'))
  }
}

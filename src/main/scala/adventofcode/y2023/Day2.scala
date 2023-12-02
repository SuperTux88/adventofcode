package adventofcode.y2023

import scala.io.BufferedSource

object Day2 extends Year2023 {
  override val day = 2

  private val GameRE = """Game (\d+): (.*)""".r
  private val ColorRE = """(\d+) (red|green|blue)""".r

  private val MAX_RED = 12
  private val MAX_GREEN = 13
  private val MAX_BLUE = 14

  override def runDay(input: BufferedSource): Unit = {
    val games = input.getLines().takeWhile(_.nonEmpty).map {
      case GameRE(id, game) => Game(id.toInt, game.split("[;,] ").map {
        case ColorRE(count, "red") => Red(count.toInt)
        case ColorRE(count, "green") => Green(count.toInt)
        case ColorRE(count, "blue") => Blue(count.toInt)
      }.toSeq)
    }.toSeq

    val valid = games.filter(_.isValid)
    printDayPart(1, valid.map(_.id).sum, "Sum of IDs of valid games: %s")

    val allPowerOfMinimalCubes = games.map(_.getPowerOfMinimalCubes)
    printDayPart(2, allPowerOfMinimalCubes.sum, "Sum of the power of minimal cubes: %s")
  }

  private case class Game(id: Int, cubes: Seq[Color]) {
    def isValid: Boolean =
      cubes.forall {
        case Red(count) => count <= MAX_RED
        case Green(count) => count <= MAX_GREEN
        case Blue(count) => count <= MAX_BLUE
      }

    def getPowerOfMinimalCubes: Int =
      cubes.groupBy(_.getClass).values.map(_.map(_.count).max).product
  }

  private sealed trait Color {
    val count: Int
  }
  private case class Red(count: Int) extends Color
  private case class Green(count: Int) extends Color
  private case class Blue(count: Int) extends Color
}

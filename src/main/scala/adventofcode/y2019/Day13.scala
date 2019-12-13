package adventofcode.y2019

import adventofcode.Logging
import adventofcode.common.Pos

import scala.annotation.tailrec

object Day13 extends Year2019 {
  override val day = 13

  private val debug = Logging.debug
  Logging.debug = false

  private val SCORE_POS = Pos(-1, 0)

  private val intcode = new IntCode(input.mkString).setMemory(0, 2)

  private val initGame = intcode.run()

  if (debug) printMap(parseOutput(initGame.output))

  printDayPart(1, countBlocks(parseOutput(initGame.output)))
  printDayPart(2, play(initGame))

  @tailrec
  private def play(game: IntCode): Int = {
    val state = parseOutput(game.output)
    if (game.isRunning) {
      play(game.run(getHorizontalPos(state, 4).compareTo(getHorizontalPos(state, 3))))
    } else {
      if (debug) printMap(state)
      state(SCORE_POS)
    }
  }

  private def parseOutput(output: Vector[Long]) =
    output.grouped(3).map {
      case Vector(x, y, id) => Pos(x.toInt, y.toInt) -> id.toInt
    }.toMap

  private def countBlocks(map: Map[Pos, Int]) = map.count(_._2 == 2)
  private def getHorizontalPos(map: Map[Pos, Int], id: Int) = map.find(_._2 == id).get._1.x

  private def printMap(map: Map[Pos, Int]): Unit =
    (0 to map.keys.map(_.y).max).foreach(y =>
      println((0 to map.keys.map(_.x).max).map(x => map(Pos(x, y)) match {
        case 0 => " "
        case 1 => "█"
        case 2 => "#"
        case 3 => "‾"
        case 4 => "o"
      }).mkString)
    )
}

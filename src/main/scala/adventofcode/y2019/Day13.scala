package adventofcode.y2019

import adventofcode.Logging
import adventofcode.common.pos.Pos
import org.jline.terminal.TerminalBuilder
import org.jline.utils.NonBlockingReader

import scala.annotation.tailrec

object Day13 extends Year2019 {
  override val day = 13

  private val SCORE_POS = Pos(-1, 0)

  override def runDay(input: IntCode): Unit = {
    val intCode = input.setMemory(0, 2)

    val initGame = intCode.run()
    val initMap = parseOutput(initGame.output)

    if (options.interactive) {
      println("\u001b[2J\u001B[25;0HUse 'a', 's' and 'd' key to move left or stay where you are or move right.")
      val terminal = TerminalBuilder.terminal
      try {
        terminal.enterRawMode()
        val reader = terminal.reader()
        try {
          playInteractive(intCode, initMap, reader)
        } finally if (reader != null) reader.close()
      } finally if (terminal != null) terminal.close()
    } else {
      if (Logging.debug) {
        printMap(initMap)
        if (!options.quiet) print("\u001b[2J")
      }

      val endScore = play(initGame, initMap)
      printDayPart(1, countBlocks(initMap), "blocks in game: %s")
      printDayPart(2, endScore, "end score: %s")
    }
  }

  @tailrec
  private def play(game: IntCode, map: Map[Pos, Int]): Int = {
    val state = parseOutput(game.output, map)
    if (Logging.debug && !options.quiet) printState(state)

    if (game.isRunning)
      play(game.run(getHorizontalPos(state, 4).compareTo(getHorizontalPos(state, 3))), state)
    else
      state(SCORE_POS)
  }

  @tailrec
  private def playInteractive(game: IntCode, map: Map[Pos, Int], reader: NonBlockingReader): Unit = {
    val state = parseOutput(game.output, map)

    if (game.isRunning) {
      printState(state)

      val input = reader.read() match {
        case 'a' => -1
        case 's' => 0
        case 'd' => 1
        case _ => 0
      }

      playInteractive(game.run(input), state, reader)
    } else {
      println("GAME OVER!!!")
      System.exit(1)
    }
  }
  private def parseOutput(output: Iterator[Long], map: Map[Pos, Int] = Map.empty) =
    map ++ output.grouped(3).map {
      case Seq(x, y, id) => Pos(x.toInt, y.toInt) -> id.toInt
    }

  private def countBlocks(map: Map[Pos, Int]) = map.count(_._2 == 2)
  private def getHorizontalPos(map: Map[Pos, Int], id: Int) = map.find(_._2 == id).get._1.x

  private def printState(state: Map[Pos, Int]): Unit = {
    print("\u001B[0;0H")
    printMap(state)
    println(s"Score: ${state(SCORE_POS)}")
  }

  private def printMap(map: Map[Pos, Int]): Unit =
    Pos.printMap(map.withDefaultValue(0), {
      case 0 => ' '
      case 1 => '█'
      case 2 => '#'
      case 3 => '‾'
      case 4 => 'o'
    }, Some(Pos(0, 0)))
}

package adventofcode.y2019

import adventofcode.Logging
import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.tools.jline.console.ConsoleReader

object Day13 extends Year2019 {
  override val day = 13

  private val SCORE_POS = Pos(-1, 0)

  override def runDay(input: IntCode): Unit = {
    val intCode = input.setMemory(0, 2)

    val initGame = intCode.run()
    val initMap = parseOutput(initGame.output)

    if (options.interactive) {
      val con = new tools.jline.console.ConsoleReader()
      println("\u001b[2J\u001B[25;0HUse 'a', 's' and 'd' key to move left or stay where you are or move right.")
      playInteractive(intCode, initMap, con)
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
  private def playInteractive(game: IntCode, map: Map[Pos, Int], con: ConsoleReader): Int = {
    val state = parseOutput(game.output, map)

    if (game.isRunning) {
      printState(state)

      val input = con.readCharacter() match {
        case 97 => -1
        case 115 => 0
        case 100 => 1
        case _ => 0
      }

      playInteractive(game.run(input), state, con)
    } else {
      println("GAME OVER!!!")
      System.exit(1).asInstanceOf[Int]
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

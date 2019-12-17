package adventofcode.y2019

import adventofcode.Logging
import adventofcode.common.Pos

import scala.annotation.tailrec

object Day17 extends Year2019 {
  override val day = 17

  private val directions = List('^', '>', 'v', '<')

  private val intCode = new IntCode(input.mkString).setMemory(0, 2)
  private val initRobot = intCode.run()
  private val initOutput = initRobot.output.map(_.toChar).mkString
  if (Logging.debug) print(initOutput)

  private val map = initOutput.split('\n').takeWhile(_.nonEmpty).zipWithIndex.flatMap {
    case (line, y) => line.zipWithIndex.map {
      case (char, x) => Pos(x, y) -> char
    }
  }.toMap

  private val (robotStartPos, directionChar) = map.find(coord => directions.contains(coord._2)).get
  private val scaffolds = map.filter(_._2 == '#').keys.toSet
  private val intersections = scaffolds.filter(scaffold => Pos.directions.forall(dir => scaffolds.contains(scaffold + dir)))

  private val path = getPath(robotStartPos, directions.indexOf(directionChar), scaffolds)
  if (Logging.debug) println(s"Path: $path")

  // inputs solved by hand ... TODO: generate this from the path to work for other inputs
  private val inputs = List("A,B,A,C,A,A,C,B,C,B", "L,12,L,8,R,12", "L,10,L,8,L,12,R,12", "R,12,L,8,L,10", "n")

  private val output = inputs.foldLeft((initRobot, List.empty[Long])) {
    case ((robot, _), nextInput) => sendInput(robot, nextInput)
  }._2
  if (Logging.debug) println()

  printDayPart(1, intersections.map(pos => pos.x * pos.y).sum)
  printDayPart(2, output.last)

  @tailrec
  private def getPath(currentPos: Pos, currentDir: Int, todo: Set[Pos], commands: String = ""): String = {
    val (turnCommand, newDir) = turn(currentPos, currentDir)
    val walkedPath = moveForward(currentPos, newDir)
    val newCommands = commands + turnCommand + walkedPath.length
    val newTodo = todo -- walkedPath

    if (newTodo.isEmpty)
      newCommands
    else
      getPath(walkedPath.last, newDir, newTodo, newCommands + ",")
  }

  private def turn(pos: Pos, dir: Int): (String, Int) = {
    if (scaffolds.contains(pos.moveDirectionIndex((dir + 1) % 4))) {
      ("R,", (dir + 1) % 4)
    } else if (scaffolds.contains(pos.moveDirectionIndex((dir + 3) % 4))) {
      ("L,", (dir + 3) % 4)
    } else {
      ("", dir)
    }
  }

  private def moveForward(pos: Pos, dir: Int): List[Pos] =
    Iterator.iterate(pos)(_.moveDirectionIndex(dir)).drop(1).takeWhile(scaffolds.contains).toList

  private def sendInput(robot: IntCode, input: String): (IntCode, List[Long]) = {
    val newRobot = robot.run(input.map(_.toLong).toVector :+ '\n'.toLong)
    val output = newRobot.output.toList

    if (Logging.debug) {
      println(input)
      print(output.map(_.toChar).mkString)
    }

    (newRobot, output)
  }
}

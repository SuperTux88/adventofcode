package adventofcode.y2022

import adventofcode.common.pos.Direction.DirectionPos
import adventofcode.common.pos.{Direction, Pos}

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day22 extends Year2022 {
  override val day = 22

  private val CONNECTIONS = List(
    (List(Pos(-1, -1), Pos(-1, 0)), 3), // left 1
    (List(Pos(1, -1), Pos(1, 0)), 1), // right 1
    (List(Pos(-2, -1), Pos(-2, 0), Pos(-1, 0)), 2), // left 2 up
    (List(Pos(-2, 1), Pos(-1, 1), Pos(0, 1)), 2), // left 2 down
    (List(Pos(2, -1), Pos(2, 0), Pos(1, 0)), 2), // right 2 up
    (List(Pos(2, 1), Pos(1, 1), Pos(0, 1)), 2), // right 2 down
    (List(Pos(-1, 3), Pos(-1, 2), Pos(0, 2), Pos(0, 1)), 1), // left 3
    (List(Pos(3, 1), Pos(2, 1), Pos(1, 1), Pos(1, 0)), 3), // right 3
    (List(Pos(2, 3)), 0), // right 4
    (List(Pos(-2, 3)), 0), // left 4
  )

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()
    val map = Pos.parseMap(lines.takeWhile(_.nonEmpty), identity)
      .filter(_._2 != ' ').view.mapValues(_ == '#').toMap
    val instructions = parseInstructions(lines.next())
    val start = map.keys.filter(_.y == 0).minBy(_.x)

    val (target, dir) = findFinalPosition(map, start, 1, instructions, wrapFlat(map))
    printDayPart(1, calcPassword(target, dir), "Final password: %s")

    val cubeSize = if map.keys.maxBy(_.y).y > 50 then 50 else 4 // detect example input
    val cube = map.keys.map(_ / cubeSize).toSet

    val (target2, dir2) = findFinalPosition(map, start, 1, instructions, wrapCube(cubeSize, cube))
    printDayPart(2, calcPassword(target2, dir2), "Final password with cube: %s")
  }

  @tailrec
  private def parseInstructions(instructions: String, result: List[Instruction] = List.empty): List[Instruction] =
    if (instructions.nonEmpty) {
      instructions.head match {
        case 'L' => parseInstructions(instructions.tail, TurnLeft() :: result)
        case 'R' => parseInstructions(instructions.tail, TurnRight() :: result)
        case _ =>
          val (num, rest) = instructions.span(_.isDigit)
          parseInstructions(rest, Move(num.toInt) :: result)
      }
    } else result.reverse

  private def findFinalPosition(map: Map[Pos, Boolean], start: Pos, direction: Int, instructions: List[Instruction],
                                wrap: (Pos, Int) => (Pos, Int)): (Pos, Int) = {
    @tailrec
    def moveSteps(pos: Pos, dir: Int, steps: Int): (Pos, Int) = {
      if (steps > 0) {
        val movedPos = pos.moveDirectionIndex(dir)
        val (wrappedPos, wrappedDir) = if (!map.contains(movedPos)) wrap(movedPos, dir) else (movedPos, dir)
        if (map(wrappedPos))
          (pos, dir)
        else
          moveSteps(wrappedPos, wrappedDir, steps - 1)
      } else (pos, dir)
    }

    @tailrec
    def move(pos: Pos, direction: Int, instructions: List[Instruction]): (Pos, Int) =
      if (instructions.nonEmpty) {
        instructions.head match {
          case TurnLeft() => move(pos, Direction.rotateLeft(direction), instructions.tail)
          case TurnRight() => move(pos, Direction.rotateRight(direction), instructions.tail)
          case Move(steps) =>
            val (newPos, newDir) = moveSteps(pos, direction, steps)
            move(newPos, newDir, instructions.tail)
        }
      } else (pos + (1, 1), (direction + 3) % 4)

    move(start, direction, instructions)
  }

  private def wrapFlat(map: Map[Pos, Boolean])(pos: Pos, dir: Int) =
    dir match {
      case 0 => (Pos(pos.x, map.keys.filter(_.x == pos.x).maxBy(_.y).y), dir)
      case 1 => (Pos(map.keys.filter(_.y == pos.y).minBy(_.x).x, pos.y), dir)
      case 2 => (Pos(pos.x, map.keys.filter(_.x == pos.x).minBy(_.y).y), dir)
      case 3 => (Pos(map.keys.filter(_.y == pos.y).maxBy(_.x).x, pos.y), dir)
    }

  private def wrapCube(cubeSize: Int, cube: Set[Pos])(pos: Pos, dir: Int): (Pos, Int) = {
    val oldPos = pos.moveDirectionIndex((dir + 2) % 4)
    val posOnOldSide = oldPos % cubeSize
    val oldSide = oldPos / cubeSize

    CONNECTIONS.map { case (connection, rotate) =>
      (connection.map(oldSide + _.rotate(dir)), rotate)
    }.collectFirst {
      case (connection, rotate) if connection.forall(cube.contains) =>
        val newDir = (dir + rotate) % 4
        val rotatedSide = rotate match {
          case 0 => posOnOldSide
          case 1 => posOnOldSide.rotateRight + (cubeSize - 1, 0)
          case 2 => posOnOldSide * -1 + (cubeSize - 1, cubeSize - 1)
          case 3 => posOnOldSide.rotateLeft + (0, cubeSize - 1)
        }
        val gluedSide = connection.head.moveDirectionIndex(Direction.flip(newDir))
        val newPos = (gluedSide * cubeSize + rotatedSide).moveDirectionIndex(newDir)
        (newPos, newDir)
    }.get
  }

  private def calcPassword(pos: Pos, dir: Int) = 1000 * pos.y + 4 * pos.x + dir

  private sealed trait Instruction
  private case class Move(steps: Int) extends Instruction
  private case class TurnLeft() extends Instruction
  private case class TurnRight() extends Instruction
}

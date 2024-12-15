package adventofcode.y2024

import adventofcode.common.pos.Pos

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day15 extends Year2024 {
  override val day = 15

  import adventofcode.y2024.Day15.Item.*

  private enum Item {
    case Robot, Box, Wall, Empty
    case BoxLeft, BoxRight // part 2
  }

  override def runDay(input: BufferedSource): Unit = {
    val lines = input.getLines()
    val map = Pos.parseMap(lines.takeWhile(_.nonEmpty), {
      case '@' => Robot
      case 'O' => Box
      case '#' => Wall
      case '.' => Empty
      case _ => throw new IllegalArgumentException("Invalid character")
    })
    val instructions = lines.mkString
    val robot = map.find { case (_, item) => item == Robot }.get._1
    val mapWithoutRobot = map.updated(robot, Empty)

    val (finalMap, finalPos) = instructions.foldLeft((mapWithoutRobot, robot))(doMove)
    printMap(finalMap.updated(finalPos, Robot))

    val boxesGPS = finalMap.filter(_._2 == Box).map { case (pos, _) => pos.y * 100 + pos.x }
    printDayPart(1, boxesGPS.sum, "Sum of all boxes GPS coordinates: %s")

    val wideMap = mapWithoutRobot.foldLeft(Map.empty[Pos, Item]) { case (map, (pos, item)) =>
      val newPos = pos * Pos(2, 1)
      (item: @unchecked) match {
        case Wall | Empty => map.updated(newPos, item).updated(newPos.right, item)
        case Box => map.updated(newPos, BoxLeft).updated(newPos.right, BoxRight)
      }
    }
    val robot2 = robot * Pos(2, 1)

    val (finalMap2, finalPos2) = instructions.foldLeft((wideMap, robot2))(doMove)
    printMap(finalMap2.updated(finalPos2, Robot))

    val boxesGPS2 = finalMap2.filter(_._2 == BoxLeft).map { case (pos, _) => pos.y * 100 + pos.x }
    printDayPart(2, boxesGPS2.sum, "Sum of all wide boxes GPS coordinates: %s")
  }

  private def doMove(current: (Map[Pos, Item], Pos), instruction: Char): (Map[Pos, Item], Pos) = {
    val (map, pos) = current
    val (newMap, newPos) = move(map, pos, instruction)
    // println(s"Move $instruction:")
    // printMap(newMap.updated(newPos, Robot))
    // println()
    (newMap, newPos)
  }

  private def move(map: Map[Pos, Item], pos: Pos, direction: Char): (Map[Pos, Item], Pos) = {
    val newPos = pos.move(direction)
    (map(newPos): @unchecked) match {
      case Wall => (map, pos)
      case Empty => (map, newPos)
      case Box =>
        findBoxesLine(map, newPos, direction) match {
          case None => (map, pos)
          case Some(boxes) => (
            map.updated(boxes.head.move(direction), Box).updated(newPos, Empty),
            newPos
          )
        }
      case BoxLeft | BoxRight =>
        direction match {
          case '<' | '>' => findBoxesLine(map, newPos, direction)
          case '^' | 'v' => findBoxesWide(map, newPos, direction)
        } match {
          case None => (map, pos)
          case Some(boxes) => (
            boxes.foldLeft(map)((m, box) => m.updated(box.move(direction), m(box)).updated(box, Empty)),
            newPos
          )
        }
    }
  }

  private def findBoxesLine(map: Map[Pos, Item], pos: Pos, direction: Char): Option[List[Pos]] = {
    val boxes = Iterator.iterate(pos)(_.move(direction)).takeWhile(p => map(p) match {
      case Box | BoxLeft | BoxRight => true
      case _ => false
    }).toList.reverse
    (map(boxes.head.move(direction)): @unchecked) match {
      case Wall => None
      case Empty => Some(boxes)
    }
  }

  private def findBoxesWide(map: Map[Pos, Item], pos: Pos, direction: Char): Option[List[Pos]] = {
    def wideBox(pos: Pos): Set[Pos] = (map(pos): @unchecked) match {
      case BoxLeft => Set(pos, pos.right)
      case BoxRight => Set(pos.left, pos)
    }

    @tailrec
    def findBoxes(current: Set[Pos], boxes: List[Pos]): Option[List[Pos]] = {
      val next = current.map(_.move(direction)).filterNot(p => map(p) == Empty)

      if (next.isEmpty) Some(current.toList ::: boxes)
      else if (next.exists(p => map(p) == Wall)) None
      else findBoxes(next.flatMap(wideBox), current.toList ::: boxes)
    }

    findBoxes(wideBox(pos), List.empty)
  }

  private def printMap(map: Map[Pos, Item]): Unit = if (adventofcode.Logging.debug)
    Pos.printMap(map, {
      case Robot => "@"
      case Box => "O"
      case Wall => "#"
      case Empty => "."

      case BoxLeft => "["
      case BoxRight => "]"
    })
}

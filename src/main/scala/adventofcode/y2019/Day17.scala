package adventofcode.y2019

import adventofcode.common.pos.Pos

object Day17 extends Year2019 {
  override val day = 17

  IntCode.printDelay = 1
  IntCode.printAsciiIn = IntCode.printAsciiIn && !options.quiet
  IntCode.printAsciiOut = IntCode.printAsciiOut && !options.quiet

  private val directions = List('^', '>', 'v', '<')

  private val intCode = new IntCode(inputString).setMemory(0, 2)
  private val (initRobot, initOutput) = intCode.startAsciiProgram()

  private val map = initOutput.split('\n').takeWhile(_.nonEmpty).zipWithIndex.flatMap {
    case (line, y) => line.zipWithIndex.map {
      case (char, x) => Pos(x, y) -> char
    }
  }.toMap

  private val (robotStartPos, directionChar) = map.find(coord => directions.contains(coord._2)).get
  private val scaffolds = map.filter(_._2 == '#').keys.toSet
  private val intersections = scaffolds.filter(scaffold => Pos.directions.forall(dir => scaffolds.contains(scaffold + dir)))

  private val path = getPath(robotStartPos, directions.indexOf(directionChar))

  private val functions = getSubPathFunctions(List(path)).head.toMap
  private val mainRoutine = getMainRoutine(path, functions.map(f => f._2.toVector -> f._1))
  private val inputs = mainRoutine.mkString(",") +: functions.map(_._2.mkString(",")).toVector :+ "n"

  private val output = inputs.foldLeft((initRobot, List.empty[Long])) {
    case ((robot, _), nextInput) => robot.sendAsciiInput(nextInput)
  }._2

  printDayPart(1, intersections.map(pos => pos.x * pos.y).sum)
  printDayPart(2, output.last, "collected dust: %s")

  private def getPath(pos: Pos, dir: Int): List[String] = {
    if (scaffolds.contains(pos.moveDirectionIndex(dir))) {
      val walkedPath = Iterator.iterate(pos)(_.moveDirectionIndex(dir)).drop(1).takeWhile(scaffolds.contains).toList
      walkedPath.length.toString :: getPath(walkedPath.last, dir)
    } else if (scaffolds.contains(pos.moveDirectionIndex((dir + 1) % 4))) {
      "R" :: getPath(pos, (dir + 1) % 4)
    } else if (scaffolds.contains(pos.moveDirectionIndex((dir + 3) % 4))) {
      "L" :: getPath(pos, (dir + 3) % 4)
    } else {
      Nil
    }
  }

  private def getSubPathFunctions(pathParts: Seq[List[String]], functions: Seq[String] = Seq("A", "B", "C")): Seq[List[(String, List[String])]] = {
    def splitListAtSlice(seq: List[String], separator: List[String]): List[List[String]] = {
      val index = seq.indexOfSlice(separator)
      if (index < 0)
        List(seq)
      else
        seq.take(index) :: splitListAtSlice(seq.drop(index + separator.length), separator)
    }

    if (pathParts.isEmpty) {
      Seq(Nil) // all parts were splitted into functions, return a non-empty list with Nil to end the functions-map
    } else if (functions.isEmpty) {
      Seq.empty // still parts leftover but no functions available anymore, return empty list to break the loop
    } else {
      for {
        elements <- 1 to 10 // with commas there are at most 10 elements in a sub-path
        subPath = pathParts.head.take(elements)
        if subPath.mkString(",").length <= 20 // check if length isn't more than 20 bytes
        otherParts = pathParts.flatMap(splitListAtSlice(_, subPath).filterNot(_.isEmpty))

        // only loop over possibilities where the remaining parts can still split into the remaining functions
        otherSubParts <- getSubPathFunctions(otherParts, functions.tail)
      } yield (functions.head -> subPath) :: otherSubParts
    }
  }

  private def getMainRoutine(path: List[String], functions: Map[Vector[String], String]) =
    path.foldLeft(List.empty[String], Vector.empty[String]) {
      case ((mainRoutine, currentSubPath), element) =>
        val newSubPath = currentSubPath :+ element
        if (functions.contains(newSubPath))
          (functions(newSubPath) :: mainRoutine, Vector.empty)
        else
          (mainRoutine, newSubPath)
    }._1.reverse
}

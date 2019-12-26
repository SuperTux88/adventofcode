package adventofcode.y2019

import adventofcode.Logging
import adventofcode.common.Dijkstra

import scala.annotation.tailrec
import scala.io.StdIn

object Day25 extends Year2019 {
  override val day = 25

  private val ListRE = """- ([\w ]+)""".r
  private val RoomRE =
    """== ([\w- ]+) ==
      |.*
      |
      |Doors here lead:
      |((?:- \w+
      |)+)(?:
      |Items here:
      |((?:- [\w ]+
      |)+))?
      |""".stripMargin.r.unanchored
  private val TakeRE = """You take the ([\w ]+)""".r.unanchored
  private val PasswordRE = """typing (\d+) on the keypad""".r.unanchored

  private val badItems = List("infinite loop") // this item starts an infinite loop in the intcode and I don't detect this at the moment

  private val intCode = new IntCode(inputString)
  private val (initDroid, initOutput) = intCode.startAsciiProgram()

  if (util.Properties.propIsSet("interactive")) {
    IntCode.printAsciiIn = false
    runInteractive(initDroid)
  } else {
    IntCode.printAsciiOut = false
    IntCode.printAsciiIn = false
    if (Logging.debug) print("Finding the shortest route ...")

    val initialRoom = parseRoom(initOutput).get
    val shortestSolution = Dijkstra(
      State(initialRoom.name, Set.empty)(initDroid, None, initialRoom),
      { state: State => state.room.password.nonEmpty },
      getNeighbors
    )

    if (Logging.debug) {
      print("\r")
      IntCode.printAsciiOut = true
      IntCode.printAsciiIn = true
      shortestSolution._2.reverse.flatMap(_.command).foldLeft(initDroid)((droid, command) => droid.sendAsciiInput(command)._1)
      println
    }

    printDayPart(1, shortestSolution._2.head.room.password.get, "password for the main airlock is: %s")
  }

  private def getNeighbors(state: State): List[(Int, State)] = {
    val nextDoors = state.room.doors.flatMap { door =>
      val (newDroid, output) = state.droid.sendAsciiInput(door)
      parseRoom(output.map(_.toChar).mkString) match {
        case Some(room: Room) => Some(State(room.name, state.inv)(newDroid, Some(door), room))
        case None => None
      }
    }
    val collectItems = state.room.items.filterNot(badItems.contains).flatMap { item =>
      val (newDroid, output) = state.droid.sendAsciiInput(s"take $item")
      if (newDroid.isRunning)
        output.map(_.toChar).mkString match {
          case TakeRE(take) =>
            val updatedRoom = state.room.copy(items = state.room.items - take)
            Some(State(state.roomName, state.inv + take)(newDroid, Some(s"take $item"), updatedRoom))
        }
      else None
    }
    (nextDoors ++ collectItems).map(state => (1, state)).toList
  }

  private def parseRoom(output: String) =
    output match {
      case RoomRE(room, doors, items) =>
        val doorsSet = doors.split('\n').map {
          case ListRE(door) => door
        }.toSet

        val itemsSet =
          if (items != null)
            items.split('\n').map {
              case ListRE(item) => item
            }.toSet
          else
            Set.empty[String]

        val passwordOption = PasswordRE.findFirstMatchIn(output) match {
          case Some(passwordMatch) => Some(passwordMatch.group(1))
          case None => None
        }

        Some(Room(room, doorsSet, itemsSet, passwordOption))
      case _ => None
    }

  @tailrec
  private def runInteractive(droid: IntCode, commands: List[String] = List.empty): Unit =
    if (droid.isRunning) {
      val command = StdIn.readLine()
      if (command != "quit" || command != null)
        runInteractive(droid.sendAsciiInput(command)._1, command :: commands)
    }

  private case class Room(name: String, doors: Set[String], items: Set[String], password: Option[String])
  private case class State(roomName: String, inv: Set[String])(val droid: IntCode, val command: Option[String], val room: Room)
}

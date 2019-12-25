package adventofcode.y2019

import scala.annotation.tailrec
import scala.io.StdIn

object Day25 extends Year2019 {
  override val day = 25

  private val PasswordRE = """.* typing (\d+) on the keypad .*""".r

  private val intCode = new IntCode(inputString)
  private val (initDroid, _) = intCode.startAsciiProgram()

  if (util.Properties.propIsSet("interactive")) {
    IntCode.printAsciiIn = false
    runInteractive(initDroid)
  } else {
    // commands to walk through ship and collect all items, found by playing interactively and only works for this input
    // bad items: "photons", "giant electromagnet", "molten lava", "escape pod", "infinite loop"
    // TODO: automate this part so it works for all inputs
    val commands = List("north", "take astronaut ice cream", "south", "west", "take mouse", "north", "take ornament", "west", "north", "take easter egg", "east", "take hypercube", "west", "north", "west", "north", "take wreath", "south", "east", "east", "south", "east", "north", "east", "take prime number", "west", "south", "west", "south", "west", "take mug", "west")
    val afterCommands = commands.foldLeft(initDroid)((droid, command) => droid.sendAsciiInput(command)._1)

    val items = afterCommands.sendAsciiInput("inv")._2.map(_.toChar).mkString.split("\n")
      .drop(2).takeWhile(_.startsWith("- ")).map(_.substring(2)).toSet

    import scala.util.control.Breaks._
    breakable {
      items.subsets.map(_.toList).toList.foreach { toDrop =>
        val afterDrop = toDrop.foldLeft(afterCommands)((droid, item) => droid.sendAsciiInput(s"drop $item")._1)
        val (afterDoor, lastOutput) = afterDrop.sendAsciiInput("north")
        if (!afterDoor.isRunning) {
          val password = lastOutput.map(_.toChar).mkString.split("\n").flatMap {
            case PasswordRE(password) => Some(password.toInt)
            case _ => None
          }.head
          printDayPart(1, password, "password for the main airlock is: %s")
          break
        }
      }
    }
  }

  @tailrec
  private def runInteractive(droid: IntCode, commands: List[String] = List.empty): Unit = {
    if (droid.isRunning) {
      val command = StdIn.readLine()
      if (command == "quit" || command == null)
        System.exit(0)
      else
        runInteractive(droid.sendAsciiInput(command)._1, command :: commands)
    }
  }
}

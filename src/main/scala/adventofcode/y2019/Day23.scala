package adventofcode.y2019

import scala.annotation.tailrec

object Day23 extends Year2019 {
  override val day = 23

  override def runDay(intCode: IntCode): Unit = {
    val computers = (0 to 49).map(address => address -> intCode.run(address)).toMap

    val (newComputers, toNAT) = runUntilIdle(computers)

    printDayPart(1, toNAT.last, "first Y for address 255: %s")
    printDayPart(2, runUntilDuplicate(newComputers, toNAT), "first duplicate Y from NAT: %s")
  }

  @tailrec
  private def runUntilIdle(computers: Map[Int, IntCode], messages: Map[Long, Vector[Long]] = Map.empty): (Map[Int, IntCode], Vector[Long]) = {
    val (newComputers, newMessages) = computers.foldLeft(computers, messages) {
      case ((newComputers, messages), (address, computer)) =>
        val newComputer = computer.run(messages.getOrElse(address, Vector(-1L)))
        (
          newComputers + (address -> newComputer),
          newComputer.output.grouped(3).foldLeft(messages) {
            case (messages, Seq(to, x, y)) =>
              if (messages.contains(to))
                messages - address + (to -> (messages(to) :+ x :+ y))
              else
                messages - address + (to -> Vector(x, y))
          }
        )
    }
    if (messages.keys == Set(255L))
      (newComputers, messages(255L))
    else
      runUntilIdle(newComputers, newMessages)
  }

  @tailrec
  private def runUntilDuplicate(computers: Map[Int, IntCode], fromNAT: Vector[Long], seen: Set[Long] = Set.empty): Long = {
    val (newComputers, toNAT) = runUntilIdle(computers, Map(0L -> fromNAT))

    if (seen.contains(toNAT.last))
      toNAT.last
    else
      runUntilDuplicate(newComputers, toNAT.takeRight(2), seen + toNAT.last)
  }
}

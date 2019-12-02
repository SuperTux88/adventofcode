package adventofcode.y2019

import scala.annotation.tailrec

object Day2 extends Year2019 {
  override val day = 2

  val intcode = input.mkString.split(",").map(_.toInt).toList

  printDayPart(1, runProgram(intcode, 12, 2))

  val (noun, verb) = (
    for {
      noun <- 0 to 99
      verb <- 0 to 99
      if runProgram(intcode, noun, verb) == 19690720
    } yield (noun, verb)
  ).head

  printDayPart(2, 100 * noun + verb)

  private def runProgram(intcode: List[Int], noun: Int, verb: Int) =
    run(intcode.updated(1, noun).updated(2, verb)).head

  @tailrec
  private def run(memory: List[Int], ip: Int = 0): List[Int] = {
    memory.slice(ip, ip + 4) match {
      case List(1, p1, p2, p3) => run(memory.updated(p3, memory(p1) + memory(p2)), ip + 4)
      case List(2, p1, p2, p3) => run(memory.updated(p3, memory(p1) * memory(p2)), ip + 4)
      case List(99, _*) => memory
    }
  }
}

package adventofcode.y2019

import scala.collection.parallel.CollectionConverters._

object Day2 extends Year2019 {
  override val day = 2

  override def runDay(intCode: IntCode): Unit = {
    printDayPart(1, runProgram(intCode, 12, 2).memory(0))

    val (noun, verb) = (
      for {
        noun <- (0 to 99).par
        verb <- (0 to 99).par
        if runProgram(intCode, noun, verb).memory(0) == 19690720
      } yield (noun, verb)
      ).head

    printDayPart(2, 100 * noun + verb)
  }

  private def runProgram(intCode: IntCode, noun: Int, verb: Int) =
    intCode.setMemory(1, noun).setMemory(2, verb).run()
}

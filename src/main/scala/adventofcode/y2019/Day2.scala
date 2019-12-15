package adventofcode.y2019

import scala.collection.parallel.CollectionConverters._

object Day2 extends Year2019 {
  override val day = 2

  private val intCode = new IntCode(input.mkString)

  printDayPart(1, runProgram(12, 2).memory(0))

  private val (noun, verb) = (
    for {
      noun <- (0 to 99).par
      verb <- (0 to 99).par
      if runProgram(noun, verb).memory(0) == 19690720
    } yield (noun, verb)
  ).head

  printDayPart(2, 100 * noun + verb)

  private def runProgram(noun: Int, verb: Int) =
    intCode.setMemory(1, noun).setMemory(2, verb).run()
}

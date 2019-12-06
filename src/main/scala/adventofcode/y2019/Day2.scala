package adventofcode.y2019

import scala.collection.parallel.CollectionConverters._

object Day2 extends Year2019 {
  override val day = 2

  val intcode = new IntCode(input.mkString)

  printDayPart(1, intcode.start(12, 2).memory(0))

  val (noun, verb) = (
    for {
      noun <- (0 to 99).par
      verb <- (0 to 99).par
      if intcode.start(noun, verb).memory(0) == 19690720
    } yield (noun, verb)
  ).head

  printDayPart(2, 100 * noun + verb)
}

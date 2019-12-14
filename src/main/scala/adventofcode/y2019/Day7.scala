package adventofcode.y2019

import adventofcode.Logging

import scala.annotation.tailrec

object Day7 extends Year2019 {
  override val day = 7

  Logging.debug = false

  private val intcode = new IntCode(input.mkString)

  printDayPart(1, getBestAmplifierOutput(0 to 4))
  printDayPart(2, getBestAmplifierOutput(5 to 9))

  private def getBestAmplifierOutput(phaseSettings: Seq[Int]) =
    phaseSettings.permutations.map { phaseOrder =>
      getAmplifierOutput(phaseOrder.map(intcode.run))
    }.max

  @tailrec
  private def getAmplifierOutput(amplifiers: Seq[IntCode], nextInput: Long = 0L): Long = {
    val (lastOutput, amplifierResults) = amplifiers.foldLeft((nextInput, Vector.empty[IntCode])) { (state, amplifier) =>
      val (input, results) = state
      val amplifierResult = amplifier.run(input)
      (amplifierResult.output.next, results :+ amplifierResult)
    }

    if (amplifierResults.last.isRunning) {
      getAmplifierOutput(amplifierResults, lastOutput)
    } else {
      lastOutput
    }
  }
}

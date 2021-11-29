package adventofcode.y2019

import scala.annotation.tailrec

object Day7 extends Year2019 {
  override val day = 7

  override def runDay(intCode: IntCode): Unit = {
    printDayPart(1, getBestAmplifierOutput(intCode, 0 to 4), "highest possible signal: %s")
    printDayPart(2, getBestAmplifierOutput(intCode, 5 to 9), "highest possible signal: %s")
  }

  private def getBestAmplifierOutput(intCode: IntCode, phaseSettings: Seq[Int]) =
    phaseSettings.permutations.map { phaseOrder =>
      getAmplifierOutput(phaseOrder.map(intCode.run))
    }.max

  @tailrec
  private def getAmplifierOutput(amplifiers: Seq[IntCode], nextInput: Long = 0L): Long = {
    val (lastOutput, amplifierResults) = amplifiers.foldLeft((nextInput, Vector.empty[IntCode])) { (state, amplifier) =>
      val (input, results) = state
      val amplifierResult = amplifier.run(input)
      (amplifierResult.output.next(), results :+ amplifierResult)
    }

    if (amplifierResults.last.isRunning) {
      getAmplifierOutput(amplifierResults, lastOutput)
    } else {
      lastOutput
    }
  }
}

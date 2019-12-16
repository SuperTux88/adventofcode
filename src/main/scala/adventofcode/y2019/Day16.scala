package adventofcode.y2019

object Day16 extends Year2019 {
  override val day = 16

  private val inputSignal = input.mkString.map(_.asDigit).toList

  private val message100 = (1 to 100).foldLeft(inputSignal) { (state, _) =>
    val stateOffset = 0 :: state
    state.indices.map { index =>
      val grouped = stateOffset.grouped(index + 1).toList
      val sum = grouped.drop(1).grouped(4).flatMap(_.head).sum +
        grouped.drop(3).grouped(4).flatMap(_.head).sum * -1
      sum.abs % 10
    }.toList
  }

  printDayPart(1, message100.slice(0, 8).mkString, "first eight digits: %s")

  private val messageOffset = inputSignal.slice(0, 7).mkString.toInt
  private val inputAfterOffset = List.fill(10000)(inputSignal).flatten.slice(messageOffset, inputSignal.length * 10000)

  private val message100AfterOffset = (1 to 100).foldLeft(inputAfterOffset) { (state, _) =>
    state.reverse.foldLeft(List.empty[Int]) { (newMessage, digitOfOldMessage) =>
      (newMessage.headOption.getOrElse(0) + digitOfOldMessage) % 10 :: newMessage
    }
  }

  printDayPart(2, message100AfterOffset.slice(0, 8).mkString, "message after offset: %s")
}

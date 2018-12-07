package adventofcode.y2018

object Day7 extends Year2018 {
  override val day = 7

  val InstructionRE = """Step ([A-Z]) must be finished before step ([A-Z]) can begin.""".r

  val (allSteps, allInstructions) = input.getLines()
    .foldLeft(Set[Char](), Map[Char, Set[Char]]().withDefaultValue(Set())) { (state, instruction) =>
      instruction match {
        case InstructionRE(before, after) => (
          state._1 + (before.charAt(0), after.charAt(0)),
          state._2 + (after.charAt(0) -> (state._2(after.charAt(0)) + before.charAt(0)))
        )
      }
  }

  var instructions = allInstructions

  val order = allSteps.foldLeft(List[Char]()) { (order, _) =>
    val next = allSteps.filterNot(s => instructions.contains(s) || order.contains(s)).min
    instructions = instructions.mapValues(_ - next).filter(_._2.nonEmpty)
    next :: order
  }.reverse

  printDayPart(1, order.mkString, "order: %s")

  instructions = allInstructions

  var stepsTodo = allSteps
  var workers = Map[Char, Int]()
  var duration = -1

  while (stepsTodo.nonEmpty || workers.nonEmpty) {
    val doneTasks = workers.filter(_._2 == duration).keys
    instructions = instructions.mapValues(_ -- doneTasks).filter(_._2.nonEmpty)
    workers --= doneTasks

    duration += 1

    while (workers.size < 5 && !stepsTodo.forall(instructions.contains(_))) {
      val next = stepsTodo.filterNot(instructions.contains(_)).min
      stepsTodo -= next
      workers += (next -> (next - 5 + duration))
    }
  }

  printDayPart(2, duration)
}

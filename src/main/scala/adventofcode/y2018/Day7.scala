package adventofcode.y2018

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day7 extends Year2018 {
  override val day = 7

  private val InstructionRE = """Step ([A-Z]) must be finished before step ([A-Z]) can begin.""".r

  override def runDay(input: BufferedSource): Unit = {
    val (allSteps, allInstructions) = input.getLines()
      .foldLeft(Set[Char](), Map[Char, Set[Char]]().withDefaultValue(Set())) { (state, instruction) =>
        instruction match {
          case InstructionRE(before, after) => (
            state._1 ++ Set(before.charAt(0), after.charAt(0)),
            state._2 + (after.charAt(0) -> (state._2(after.charAt(0)) + before.charAt(0)))
          )
        }
      }

    val order = allSteps.foldLeft(List[Char](), allInstructions) { (state, _) =>
      val next = allSteps.filterNot(s => state._2.contains(s) || state._1.contains(s)).min
      (next :: state._1, state._2.view.mapValues(_ - next).filter(_._2.nonEmpty).toMap)
    }._1.reverse

    printDayPart(1, order.mkString, "order: %s")

    printDayPart(2, workParallel(allSteps, allInstructions))
  }

  @tailrec
  def workParallel(stepsTodo: Set[Char], instructions: Map[Char, Set[Char]], workers: Map[Char, Int] = Map(), time: Int = 0): Int = {
    if (stepsTodo.isEmpty) {
      workers.maxBy(_._2)._2
    } else if (workers.size < 5 && !stepsTodo.forall(instructions.contains)) {
      val next = stepsTodo.filterNot(instructions.contains).min
      workParallel(stepsTodo - next, instructions, workers + (next -> (next - 4 + time)), time)
    } else {
      val newTime = workers.minBy(_._2)._2
      val doneTasks = workers.filter(_._2 == newTime).keys
      val newInstructions = instructions.view.mapValues(_ -- doneTasks).filter(_._2.nonEmpty).toMap
      workParallel(stepsTodo, newInstructions, workers -- doneTasks, newTime)
    }
  }
}

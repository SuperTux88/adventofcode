package adventofcode.y2016

import scala.util.control.Breaks._

object Day25 extends Year2016 {
  override val day = 25

  val computer = new Computer(input.getLines)

  var solution: Option[Int] = None
  var initValue = 0

  while (solution.isEmpty) {
    initValue += 1

    breakable {
      var lastOutput = -1
      var counter = 0

      computer.run("a" -> initValue, checkOutput)

      def checkOutput(output: Int) {
        if (lastOutput == output) {
          break
        } else {
          lastOutput = output
          counter += 1
          if (counter > 100) {
            solution = Some(initValue)
            break
          }
        }
      }
    }
  }

  printDayPart(1, solution.get)
}

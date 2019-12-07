package adventofcode.y2019

import adventofcode.Logging
import adventofcode.y2019.IntCode.State

import scala.annotation.tailrec
import scala.math.pow

class IntCode private(val program: Vector[Int], private val state: State) {

  def this(program: Vector[Int]) = this(program, State(program, 0, Vector.empty[Int]))
  def this(instructions: String) = this(instructions.split(",").map(_.toInt).toVector)

  def memory: Vector[Int] = state.memory
  def output: Vector[Int] = state.output
  def isRunning: Boolean = state.ip >= 0

  def run(): IntCode = run(Vector.empty)
  def run(input: Int): IntCode = run(Vector(input))
  def run(input: Vector[Int]): IntCode = new IntCode(program, run(memory, state.ip, input, output))

  @tailrec
  private def run(memory: Vector[Int], ip: Int, inputs: Vector[Int], outputs: Vector[Int] = Vector.empty): State = {
    def param(parameter: Int) = memory(ip + parameter)
    def value(parameter: Int) =
      (memory(ip) / pow(10, parameter + 1) % 10).toInt match {
        case 0 => // position mode
          memory(param(parameter))
        case 1 => // immediate mode
          param(parameter)
      }

    memory(ip) % 100 match {
      case 1 => // add
        val newMemory = memory.updated(param(3), value(1) + value(2))
        run(newMemory, ip + 4, inputs, outputs)
      case 2 => // multiply
        val newMemory = memory.updated(param(3), value(1) * value(2))
        run(newMemory, ip + 4, inputs, outputs)
      case 3 => // input
        if (inputs.isEmpty) {
          State(memory, ip, outputs)
        } else {
          run(memory.updated(param(1), inputs.head), ip + 2, inputs.tail, outputs)
        }
      case 4 => // output
        val output = value(1)
        if (Logging.debug) println(s"Output: $output")
        run(memory, ip + 2, inputs, outputs :+ output)
      case 5 => // jump-if-true
        val newIp = if (value(1) != 0) value(2) else ip + 3
        run(memory, newIp, inputs, outputs)
      case 6 => // jump-if-false
        val newIp = if (value(1) == 0) value(2) else ip + 3
        run(memory, newIp, inputs, outputs)
      case 7 => // less than
        val newMemory = memory.updated(param(3), if (value(1) < value(2)) 1 else 0)
        run(newMemory, ip + 4, inputs, outputs)
      case 8 => // equals
        val newMemory = memory.updated(param(3), if (value(1) == value(2)) 1 else 0)
        run(newMemory, ip + 4, inputs, outputs)
      case 99 => // exit
        State(memory, -1, outputs)
    }
  }
}
object IntCode {
  private case class State(memory: Vector[Int], ip: Int, output: Vector[Int])
}

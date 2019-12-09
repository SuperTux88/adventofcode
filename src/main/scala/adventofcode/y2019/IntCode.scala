package adventofcode.y2019

import adventofcode.Logging
import adventofcode.y2019.IntCode.State

import scala.annotation.tailrec
import scala.math.pow

class IntCode private(val program: Vector[Long], private val state: State) {

  def this(program: Vector[Long]) = this(program, State(program, 0, Vector.empty[Long]))
  def this(instructions: String) = this(instructions.split(",").map(_.toLong).toVector)

  def memory: Vector[Long] = state.memory
  def output: Vector[Long] = state.output
  def isRunning: Boolean = state.ip >= 0

  def run(): IntCode = run(Vector.empty)
  def run(input: Int): IntCode = run(Vector(input.toLong))
  def run(input: Long): IntCode = run(Vector(input))
  def run(input: Vector[Long]): IntCode = new IntCode(program, run(memory, state.ip, input, output))

  @tailrec
  private def run(memory: Vector[Long], ip: Int, inputs: Vector[Long], outputs: Vector[Long] = Vector.empty): State = {
    def param(parameter: Int) = memory(ip + parameter)
    def value(parameter: Int) =
      (memory(ip.toInt) / pow(10, parameter + 1) % 10).toInt match {
        case 0 => // position mode
          memory(param(parameter).toInt)
        case 1 => // immediate mode
          param(parameter)
      }

    memory(ip) % 100 match {
      case 1 => // add
        val newMemory = memory.updated(param(3).toInt, value(1) + value(2))
        run(newMemory, ip + 4, inputs, outputs)
      case 2 => // multiply
        val newMemory = memory.updated(param(3).toInt, value(1) * value(2))
        run(newMemory, ip + 4, inputs, outputs)
      case 3 => // input
        if (inputs.isEmpty) {
          State(memory, ip, outputs)
        } else {
          run(memory.updated(param(1).toInt, inputs.head), ip + 2, inputs.tail, outputs)
        }
      case 4 => // output
        val output = value(1)
        if (Logging.debug) println(s"Output: $output")
        run(memory, ip + 2, inputs, outputs :+ output)
      case 5 => // jump-if-true
        val newIp = if (value(1) != 0) value(2).toInt else ip + 3
        run(memory, newIp, inputs, outputs)
      case 6 => // jump-if-false
        val newIp = if (value(1) == 0) value(2).toInt else ip + 3
        run(memory, newIp, inputs, outputs)
      case 7 => // less than
        val newMemory = memory.updated(param(3).toInt, if (value(1) < value(2)) 1L else 0L)
        run(newMemory, ip + 4, inputs, outputs)
      case 8 => // equals
        val newMemory = memory.updated(param(3).toInt, if (value(1) == value(2)) 1L else 0L)
        run(newMemory, ip + 4, inputs, outputs)
      case 99 => // exit
        State(memory, -1, outputs)
    }
  }
}
object IntCode {
  private case class State(memory: Vector[Long], ip: Int, output: Vector[Long])
}

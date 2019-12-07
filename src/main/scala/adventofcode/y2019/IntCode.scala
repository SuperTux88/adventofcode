package adventofcode.y2019

import scala.annotation.tailrec
import scala.math.pow

class IntCode(val program: Vector[Int]) {

  def this(instructions: String) = this(instructions.split(",").map(_.toInt).toVector)

  def start(input: Vector[Int] = Vector.empty): Result = run(program, 0, input)

  @tailrec
  private def run(memory: Vector[Int], ip: Int, inputs: Vector[Int], outputs: Vector[Int] = Vector.empty): Result = {
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
        run(memory.updated(param(1), inputs.head), ip + 2, inputs.tail, outputs)
      case 4 => // output
        run(memory, ip + 2, inputs, outputs :+ value(1))
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
        Result(memory, outputs)
    }
  }

  case class Result(memory: Vector[Int], output: Vector[Int])
}

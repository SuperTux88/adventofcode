package adventofcode.y2019

import scala.annotation.tailrec
import scala.math.pow

class IntCode(private val instructions: String) {

  private val program = instructions.split(",").map(_.toInt).toVector

  def start(input: Vector[Int]): Result = run(program, 0, input)

  @tailrec
  private def run(memory: Vector[Int], ip: Int = 0, inputs: Vector[Int], outputs: Vector[Int] = Vector.empty): Result = {
    memory(ip) % 100 match {
      case 1 => // add
        val newMemory = memory.updated(memory(ip + 3), getValue(memory, ip, 1) + getValue(memory, ip, 2))
        run(newMemory, ip + 4, inputs, outputs)
      case 2 => // multiply
        val newMemory = memory.updated(memory(ip + 3), getValue(memory, ip, 1) * getValue(memory, ip, 2))
        run(newMemory, ip + 4, inputs, outputs)
      case 3 => // input
        run(memory.updated(memory(ip + 1), inputs.head), ip + 2, inputs.tail, outputs)
      case 4 => // output
        run(memory, ip + 2, inputs, outputs :+ getValue(memory, ip, 1))
      case 5 => // jump-if-true
        val newIp = if (getValue(memory, ip, 1) != 0) getValue(memory, ip, 2) else ip + 3
        run(memory, newIp, inputs, outputs)
      case 6 => // jump-if-false
        val newIp = if (getValue(memory, ip, 1) == 0) getValue(memory, ip, 2) else ip + 3
        run(memory, newIp, inputs, outputs)
      case 7 => // less than
        val newMemory = memory.updated(memory(ip + 3), if (getValue(memory, ip, 1) < getValue(memory, ip, 2)) 1 else 0)
        run(newMemory, ip + 4, inputs, outputs)
      case 8 => // equals
        val newMemory = memory.updated(memory(ip + 3), if (getValue(memory, ip, 1) == getValue(memory, ip, 2)) 1 else 0)
        run(newMemory, ip + 4, inputs, outputs)
      case 99 => // exit
        Result(memory, outputs)
    }
  }

  private def getValue(memory: Seq[Int], ip: Int, parameter: Int) = {
    (memory(ip) / pow(10, parameter + 1) % 10).toInt match {
      case 0 => // position mode
        memory(memory(ip + parameter))
      case 1 => // immediate mode
        memory(ip + parameter)
    }
  }

  case class Result(memory: Vector[Int], output: Vector[Int])
}

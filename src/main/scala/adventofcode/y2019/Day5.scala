package adventofcode.y2019

import scala.annotation.tailrec
import scala.math.pow

object Day5 extends Year2019 {
  override val day = 5

  val intcode = input.mkString.split(",").map(_.toInt).toList

  printDayPart(1, run(intcode, 0, List(1))._2.head)
  printDayPart(2, run(intcode, 0, List(5))._2.head)

  @tailrec
  private def run(memory: List[Int], ip: Int = 0, inputs: List[Int], outputs: List[Int] = List()): (List[Int], List[Int]) = {
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
        run(memory, ip + 2, inputs, getValue(memory, ip, 1) :: outputs)
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
        (memory, outputs)
    }
  }

  private def getValue(memory: List[Int], ip: Int, parameter: Int) = {
    (memory(ip) / pow(10, parameter + 1) % 10).toInt match {
      case 0 => memory(memory(ip + parameter))
      case 1 => memory(ip + parameter)
    }
  }
}

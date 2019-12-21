package adventofcode.y2019

import adventofcode.Logging

import scala.annotation.tailrec
import scala.io.AnsiColor.{CYAN, RESET}
import scala.math.pow

class IntCode private(val memory: Vector[Long], ip: Int = 0, val output: Iterator[Long] = Iterator.empty, relativeBase: Long = 0) {

  def this(program: String) = this(program.split(",").map(_.toLong).toVector)

  def isRunning: Boolean = ip >= 0

  def setMemory(index: Int, value: Long): IntCode = new IntCode(memory.updated(index, value), ip, output, relativeBase)

  def startAsciiProgram(): (IntCode, String) = {
    val intCode = run()
    val output = intCode.output.toList.map(_.toChar).mkString
    if (IntCode.asciiOut) print(output)
    (intCode, output)
  }

  def sendAsciiInput(input: String): (IntCode, List[Long]) = {
    val intCode = run(input.map(_.toLong).toVector :+ '\n'.toLong)
    val output = intCode.output.toList

    if (IntCode.asciiOut) {
      println(s"$CYAN$input$RESET")
      print(output.filter(_ < 255).map(_.toChar).mkString)
    }

    (intCode, output)
  }

  def run(): IntCode = run(Vector.empty)
  def run(input: Int): IntCode = run(Vector(input.toLong))
  def run(input: Long): IntCode = run(Vector(input))
  def run(input: Vector[Long]): IntCode = run(memory, ip, input, output, relativeBase)

  @tailrec
  private def run(memory: Vector[Long], ip: Int, inputs: Vector[Long], outputs: Iterator[Long], relativeBase: Long): IntCode = {
    def param(parameter: Int) = // TODO refactor parameter modes to remove duplicate check?
      (memory(ip.toInt) / pow(10, parameter + 1) % 10).toInt match {
        case 0|1 => // position mode and immediate mode
          memory(ip + parameter)
        case 2 => // relative mode
          memory(ip + parameter) + relativeBase
      }
    def value(parameter: Int) =
      (memory(ip.toInt) / pow(10, parameter + 1) % 10).toInt match {
        case 0|2 => // position mode and relative mode
          memory.lift(param(parameter).toInt).getOrElse(0L)
        case 1 => // immediate mode
          param(parameter)
      }

    def updateMemory(position: Int, value: Long) =
      if (position < memory.length) {
        memory.updated(position, value)
      } else {
        memory ++ Vector.fill(position - memory.length)(0L) :+ value
      }

    memory(ip) % 100 match {
      case 1 => // add
        val newMemory = updateMemory(param(3).toInt, value(1) + value(2))
        run(newMemory, ip + 4, inputs, outputs, relativeBase)
      case 2 => // multiply
        val newMemory = updateMemory(param(3).toInt, value(1) * value(2))
        run(newMemory, ip + 4, inputs, outputs, relativeBase)
      case 3 => // input
        if (inputs.isEmpty) {
          new IntCode(memory, ip, outputs, relativeBase)
        } else {
          run(updateMemory(param(1).toInt, inputs.head), ip + 2, inputs.tail, outputs, relativeBase)
        }
      case 4 => // output
        val output = value(1)
        if (IntCode.debug) println(s"Output: $output")
        run(memory, ip + 2, inputs, outputs ++ Iterator(output), relativeBase)
      case 5 => // jump-if-true
        val newIp = if (value(1) != 0) value(2).toInt else ip + 3
        run(memory, newIp, inputs, outputs, relativeBase)
      case 6 => // jump-if-false
        val newIp = if (value(1) == 0) value(2).toInt else ip + 3
        run(memory, newIp, inputs, outputs, relativeBase)
      case 7 => // less than
        val newMemory = updateMemory(param(3).toInt, if (value(1) < value(2)) 1L else 0L)
        run(newMemory, ip + 4, inputs, outputs, relativeBase)
      case 8 => // equals
        val newMemory = updateMemory(param(3).toInt, if (value(1) == value(2)) 1L else 0L)
        run(newMemory, ip + 4, inputs, outputs, relativeBase)
      case 9 => // adjusts relative base
        run(memory, ip + 2, inputs, outputs, relativeBase + value(1))
      case 99 => // exit
        new IntCode(memory, -1, outputs, relativeBase)
    }
  }
}

object IntCode {
  var debug: Boolean = false
  var asciiOut: Boolean = Logging.debug
}

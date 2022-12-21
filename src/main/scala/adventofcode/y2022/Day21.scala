package adventofcode.y2022

import java.lang
import java.lang.Package
import scala.io.BufferedSource

object Day21 extends Year2022 {
  override val day = 21

  private val MonkeyNumberRE = """(\w+): (\d+)""".r
  private val MonkeyAddRE = """(\w+): (\w+) \+ (\w+)""".r
  private val MonkeySubRE = """(\w+): (\w+) - (\w+)""".r
  private val MonkeyMulRE = """(\w+): (\w+) \* (\w+)""".r
  private val MonkeyDivRE = """(\w+): (\w+) / (\w+)""".r

  private val ROOT = "root"
  private val HUMAN = "humn"

  override def runDay(input: BufferedSource): Unit = {
    val monkeys = input.getLines().map {
      case MonkeyNumberRE(name, value) if name == HUMAN => HUMAN -> Human(value.toLong)
      case MonkeyNumberRE(name, value) => name -> ValueMonkey(value.toLong)
      case MonkeyAddRE(name, left, right) => name ->
        OperationMonkey(left, right, _ + _, (l, r) => r - l, _ - _)
      case MonkeySubRE(name, left, right) => name ->
        OperationMonkey(left, right, _ - _, _ - _, _ + _)
      case MonkeyMulRE(name, left, right) => name ->
        OperationMonkey(left, right, _ * _, (l, r) => r / l, _ / _)
      case MonkeyDivRE(name, left, right) => name ->
        OperationMonkey(left, right, _ / _, _ / _, _ * _)
    }.toMap
    val root = monkeys(ROOT).asInstanceOf[OperationMonkey]

    printDayPart(1, root.value(monkeys), "Root number: %s")

    val (left, right) = (monkeys(root.left), monkeys(root.right))
    val human = (left.valueOption(monkeys), right.valueOption(monkeys)) match {
      case (Some(leftValue), None) => right.solve(monkeys, leftValue)
      case (None, Some(rightValue)) => left.solve(monkeys, rightValue)
      case _ => throw new RuntimeException("There should be always exactly one human!")
    }

    printDayPart(2, human.get, "Human number: %s")
  }

  private sealed trait Monkey {
    def value(monkeys: Map[String, Monkey]): Long
    def valueOption(monkeys: Map[String, Monkey]): Option[Long] = Some(value(monkeys))
    def solve(monkeys: Map[String, Monkey], value: Long): Option[Long]
  }

  private case class ValueMonkey(value: Long) extends Monkey {
    override def value(monkeys: Map[String, Monkey]): Long = value
    override def solve(monkeys: Map[String, Monkey], value: Long): Option[Long] =
      throw new RuntimeException("Value monkey cannot be solved!")
  }

  private case class Human(value: Long) extends Monkey {
    override def value(monkeys: Map[String, Monkey]): Long = value
    override def valueOption(monkeys: Map[String, Monkey]): Option[Long] = None
    override def solve(monkeys: Map[String, Monkey], value: Long): Option[Long] = Some(value)
  }

  private case class OperationMonkey(left: String, right: String,
                                     op: (Long, Long) => Long,
                                     solveLeft: (Long, Long) => Long,
                                     solveRight: (Long, Long) => Long
                                    ) extends Monkey {
    override def value(monkeys: Map[String, Monkey]): Long = op(monkeys(left).value(monkeys), monkeys(right).value(monkeys))

    override def valueOption(monkeys: Map[String, Monkey]): Option[Long] = for {
      leftValue <- monkeys(left).valueOption(monkeys)
      rightValue <- monkeys(right).valueOption(monkeys)
    } yield op(leftValue, rightValue)

    override def solve(monkeys: Map[String, Monkey], value: Long): Option[Long] = {
      (monkeys(left).valueOption(monkeys), monkeys(right).valueOption(monkeys)) match {
        case (Some(l), None) => monkeys(right).solve(monkeys, solveLeft(l, value))
        case (None, Some(r)) => monkeys(left).solve(monkeys, solveRight(value, r))
        case _ => throw new RuntimeException("There should be always exactly one human!")
      }
    }
  }
}
